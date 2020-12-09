package com.twilio.guardrail.generators.Java.collectionslib

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, Type, UnknownType }
import com.github.javaparser.ast.body.Parameter
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.{ BlockStmt, ExpressionStmt, IfStmt, Statement }
import com.twilio.guardrail.generators.syntax.Java._

import scala.compat.java8.OptionConverters._

sealed trait CollectionsLibType {
  def optionalSideEffect(on: Expression, sideEffectParamName: String, sideEffectBody: List[Statement]): Expression
  def optionalGetOrElse: String
  def isOptionalType(tpe: Type): Boolean
  def isArrayType(tpe: Type): Boolean

  def completionStageTermToFutureTerm(completionStageExpr: Expression): Expression
  def futureTermToCompletionStageTerm(futureExpr: Expression): Expression
  def liftFutureType(tpe: Type): Type
  def futureMap(on: Expression, resultParamName: String, mapBody: List[Statement]): Expression
  def futureSideEffect(on: Expression, resultParamName: String, resultBody: List[Statement], errorParamName: String, errorBody: List[Statement]): Expression
}

object CollectionsLibType {
  private[collectionslib] def statementsToStatement(statements: List[Statement], allowBareExpression: Boolean): Statement =
    statements match {
      case (exprStmt: ExpressionStmt) :: Nil if allowBareExpression => exprStmt
      case (blockStmt: BlockStmt) :: Nil                            => blockStmt
      case stmts                                                    => new BlockStmt(stmts.toNodeList)
    }

  private[collectionslib] def buildLambdaExpr(parameterNames: List[String], lambdaBody: List[Statement]): LambdaExpr = {
    val parameters = parameterNames.map(new Parameter(new UnknownType, _)).toNodeList
    new LambdaExpr(parameters, statementsToStatement(lambdaBody, allowBareExpression = true), parameters.size > 1)
  }

  private[collectionslib] def lambdaMethodCall(on: Expression, sideEffectParamName: String, sideEffectBody: List[Statement], methodName: String): Expression =
    new MethodCallExpr(
      on,
      methodName,
      new NodeList[Expression](buildLambdaExpr(List(sideEffectParamName), sideEffectBody))
    )

  private[collectionslib] def isContainerOfType(tpe: Type, containerClsScope: String, containerClsName: String): Boolean = tpe match {
    case cls: ClassOrInterfaceType =>
      val tpeScope = cls.getScope.asScala
      cls.getNameAsString == containerClsName && (tpeScope.isEmpty || tpeScope.map(_.asString).contains(containerClsScope))
    case _ => false
  }
}

trait JavaStdLibCollections extends CollectionsLibType {
  override def optionalSideEffect(on: Expression, sideEffectParamName: String, sideEffectBody: List[Statement]): Expression =
    CollectionsLibType.lambdaMethodCall(on, sideEffectParamName, sideEffectBody, "ifPresent")

  override def optionalGetOrElse: String          = "orElseGet"
  override def isOptionalType(tpe: Type): Boolean = CollectionsLibType.isContainerOfType(tpe, "java.util", "Optional")
  override def isArrayType(tpe: Type): Boolean    = CollectionsLibType.isContainerOfType(tpe, "java.util", "List")

  override def completionStageTermToFutureTerm(completionStageExpr: Expression): Expression = completionStageExpr
  override def futureTermToCompletionStageTerm(futureExpr: Expression): Expression          = futureExpr

  override def liftFutureType(tpe: Type): Type =
    StaticJavaParser.parseClassOrInterfaceType("java.util.concurrent.CompletionStage").setTypeArguments(tpe)

  override def futureMap(on: Expression, resultParamName: String, mapBody: List[Statement]): Expression =
    CollectionsLibType.lambdaMethodCall(on, resultParamName, mapBody, "thenApply")

  override def futureSideEffect(
      on: Expression,
      resultParamName: String,
      resultBody: List[Statement],
      errorParamName: String,
      errorBody: List[Statement]
  ): Expression =
    new MethodCallExpr(
      on,
      "whenComplete",
      new NodeList[Expression](
        CollectionsLibType.buildLambdaExpr(
          List(resultParamName, errorParamName),
          List(
            new IfStmt(
              new BinaryExpr(new NameExpr("err"), new NullLiteralExpr, BinaryExpr.Operator.NOT_EQUALS),
              CollectionsLibType.statementsToStatement(errorBody, allowBareExpression = false),
              CollectionsLibType.statementsToStatement(resultBody, allowBareExpression = false)
            )
          )
        )
      )
    )
}

trait JavaVavrCollections extends CollectionsLibType {
  override def optionalSideEffect(on: Expression, sideEffectParamName: String, sideEffectBody: List[Statement]): Expression =
    CollectionsLibType.lambdaMethodCall(on, sideEffectParamName, sideEffectBody, "forEach")

  override def optionalGetOrElse: String          = "getOrElse"
  override def isOptionalType(tpe: Type): Boolean = CollectionsLibType.isContainerOfType(tpe, "io.vavr.control", "Option")
  override def isArrayType(tpe: Type): Boolean    = CollectionsLibType.isContainerOfType(tpe, "io.vavr.collection", "Vector")

  override def completionStageTermToFutureTerm(completionStageExpr: Expression): Expression =
    new MethodCallExpr(
      new NameExpr("io.vavr.concurrent.Future"),
      "fromCompletableFuture",
      new NodeList[Expression](new MethodCallExpr(completionStageExpr, "toCompletableFuture"))
    )

  override def futureTermToCompletionStageTerm(futureExpr: Expression): Expression =
    new MethodCallExpr(futureExpr, "toCompletableFuture")

  override def liftFutureType(tpe: Type): Type =
    StaticJavaParser.parseClassOrInterfaceType("io.vavr.concurrent.Future").setTypeArguments(tpe)

  override def futureMap(on: Expression, resultParamName: String, mapBody: List[Statement]): Expression =
    CollectionsLibType.lambdaMethodCall(on, resultParamName, mapBody, "map")

  override def futureSideEffect(
      on: Expression,
      resultParamName: String,
      resultBody: List[Statement],
      errorParamName: String,
      errorBody: List[Statement]
  ): Expression =
    new MethodCallExpr(
      new MethodCallExpr(
        on,
        "onFailure",
        new NodeList[Expression](CollectionsLibType.buildLambdaExpr(List(errorParamName), errorBody))
      ),
      "onSuccess",
      new NodeList[Expression](CollectionsLibType.buildLambdaExpr(List(resultParamName), resultBody))
    )
}
