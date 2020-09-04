package com.twilio.guardrail.generators.Java.collectionslib

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, Type, UnknownType }
import com.github.javaparser.ast.body.Parameter
import com.github.javaparser.ast.expr.{ Expression, LambdaExpr, MethodCallExpr }
import com.github.javaparser.ast.stmt.{ BlockStmt, ExpressionStmt, Statement }
import com.twilio.guardrail.generators.syntax.Java._

import scala.compat.java8.OptionConverters._

sealed trait CollectionsLibType {
  def optionalSideEffect(on: Expression, sideEffectParamName: String, sideEffectBody: List[Statement]): Expression
  def optionalGetOrElse: String
  def isOptionalType(tpe: Type): Boolean
  def isArrayType(tpe: Type): Boolean
}

object CollectionsLibType {
  private[collectionslib] def lambdaMethodCall(on: Expression, sideEffectParamName: String, sideEffectBody: List[Statement], methodName: String): Expression = {
    val parameter = new Parameter(new UnknownType, sideEffectParamName)
    new MethodCallExpr(
      on,
      methodName,
      new NodeList[Expression](
        sideEffectBody match {
          case (exprStmt: ExpressionStmt) :: Nil => new LambdaExpr(parameter, exprStmt.getExpression)
          case (blockStmt: BlockStmt) :: Nil     => new LambdaExpr(parameter, blockStmt)
          case stmts                             => new LambdaExpr(parameter, new BlockStmt(stmts.toNodeList))
        }
      )
    )
  }

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
}
