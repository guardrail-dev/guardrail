package com.twilio.guardrail.terms.collections

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.Modifier.finalModifier
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ PrimitiveType, Type, UnknownType }
import com.github.javaparser.ast.body.{ Parameter, VariableDeclarator }
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.terms.collections.JavaCollectionsHelpers._
import java.util.concurrent.CompletionStage
import scala.compat.java8.OptionConverters._
import scala.concurrent.Future
import scala.reflect.ClassTag

trait JavaStdLibCollections extends CollectionsAbstraction[JavaLanguage] {
  override implicit val optionInstances: OptionF[JavaLanguage] = new OptionF[JavaLanguage] {
    override def liftType(tpe: Type): Type  = StaticJavaParser.parseClassOrInterfaceType("java.util.Optional").setTypeArguments(tpe)
    override def isType(tpe: Type): Boolean = isContainerOfType(tpe, "java.util", "Optional")

    override def pure[From <: Expression, A](fa: TermHolder[JavaLanguage, From, A]): TermHolder[JavaLanguage, MethodCallExpr, Option[A]] =
      doMethodCall(new NameExpr("java.util.Optional"), "ofNullable", fa.value)

    override def empty[A]: TermHolder[JavaLanguage, MethodCallExpr, Option[A]] =
      TermHolder[JavaLanguage, MethodCallExpr, Option[A]](new MethodCallExpr(new NameExpr("java.util.Optional"), "empty"))

    override def foreach[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Unit]
    )(fa: TermHolder[JavaLanguage, From, Option[A]]): TermHolder[JavaLanguage, MethodCallExpr, Unit] =
      doMethodCall(fa.value, "ifPresent", f.value)

    override def map[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => B]
    )(fa: TermHolder[JavaLanguage, From, Option[A]]): TermHolder[JavaLanguage, MethodCallExpr, Option[B]] =
      doMethodCall(fa.value, "map", f.value)

    override def flatMap[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Option[B]]
    )(fa: TermHolder[JavaLanguage, From, Option[A]]): TermHolder[JavaLanguage, MethodCallExpr, Option[B]] =
      doMethodCall(fa.value, "flatMap", f.value)

    override def getOrElse[From <: Expression, A, B >: A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, () => B]
    )(fa: TermHolder[JavaLanguage, From, Option[A]]): TermHolder[JavaLanguage, MethodCallExpr, B] =
      doMethodCall(fa.value, "orElseGet", f.value)

    override def getOrElseNull[From <: Expression, A](fa: TermHolder[JavaLanguage, From, Option[A]]): TermHolder[JavaLanguage, MethodCallExpr, A] =
      doMethodCall(fa.value, "orElse", new NullLiteralExpr)

    override def getOrElseThrow[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, () => B]
    )(fa: TermHolder[JavaLanguage, From, Option[A]]): TermHolder[JavaLanguage, MethodCallExpr, A] =
      doMethodCall(fa.value, "orElseThrow", f.value)
  }

  override implicit val vectorInstances: MonadF[JavaLanguage, Vector] = new MonadF[JavaLanguage, Vector] {
    override def liftType(tpe: Type): Type = StaticJavaParser.parseClassOrInterfaceType("java.util.List").setTypeArguments(tpe)
    override def isType(tpe: Type): Boolean =
      isContainerOfType(tpe, "java.util", "List") ||
        isContainerOfType(tpe, "java.util", "Vector")

    override def pure[From <: Expression, A](fa: TermHolder[JavaLanguage, From, A]): TermHolder[JavaLanguage, MethodCallExpr, Vector[A]] =
      TermHolder[JavaLanguage, MethodCallExpr, Vector[A]](
        new MethodCallExpr(
          new NameExpr("java.util.Collections"),
          "singletonList",
          new NodeList[Expression](fa.value)
        )
      )

    override def foreach[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Unit]
    )(fa: TermHolder[JavaLanguage, From, Vector[A]]): TermHolder[JavaLanguage, MethodCallExpr, Unit] =
      TermHolder[JavaLanguage, MethodCallExpr, Unit](
        new MethodCallExpr(
          fa.value,
          "forEach",
          new NodeList[Expression](f.value)
        )
      )

    // FIXME: wrapStream() and doCollect() are likely inefficient if more operations will be done
    override def map[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => B]
    )(fa: TermHolder[JavaLanguage, From, Vector[A]]): TermHolder[JavaLanguage, MethodCallExpr, Vector[B]] =
      TermHolder[JavaLanguage, MethodCallExpr, Vector[B]](
        doCollect(
          new MethodCallExpr(
            wrapStream(fa.value),
            "map",
            new NodeList[Expression](f.value)
          )
        )
      )

    // FIXME: wrapStream() and doCollect() are likely inefficient if more operations will be done
    override def flatMap[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Vector[B]]
    )(fa: TermHolder[JavaLanguage, From, Vector[A]]): TermHolder[JavaLanguage, MethodCallExpr, Vector[B]] =
      TermHolder[JavaLanguage, MethodCallExpr, Vector[B]](
        doCollect(
          new MethodCallExpr(
            wrapStream(fa.value),
            "flatMap",
            new NodeList[Expression](f.value)
          )
        )
      )
  }

  override implicit val futureInstances: FutureF[JavaLanguage] = new FutureF[JavaLanguage] {
    override def liftType(tpe: Type): Type = StaticJavaParser.parseClassOrInterfaceType("java.util.concurrent.CompletionStage").setTypeArguments(tpe)
    override def isType(tpe: Type): Boolean =
      isContainerOfType(tpe, "java.util.concurrent", "CompletionStage") ||
        isContainerOfType(tpe, "java.util.concurrent", "CompletableFuture")

    override def pure[From <: Expression, A](fa: TermHolder[JavaLanguage, From, A]): TermHolder[JavaLanguage, MethodCallExpr, Future[A]] =
      doMethodCall(new NameExpr("java.util.concurrent.CompletableFuture"), "completedFuture", fa.value)

    override def foreach[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Unit]
    )(fa: TermHolder[JavaLanguage, From, Future[A]]): TermHolder[JavaLanguage, MethodCallExpr, Unit] =
      doMethodCall(fa.value, "thenAccept", f.value)

    override def map[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => B]
    )(fa: TermHolder[JavaLanguage, From, Future[A]]): TermHolder[JavaLanguage, MethodCallExpr, Future[B]] =
      doMethodCall(fa.value, "thenApply", f.value)

    override def flatMap[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Future[B]]
    )(fa: TermHolder[JavaLanguage, From, Future[A]]): TermHolder[JavaLanguage, MethodCallExpr, Future[B]] =
      doMethodCall(fa.value, "thenCompose", f.value)

    override def fromCompletionStage[From <: Expression, A](
        fa: TermHolder[JavaLanguage, From, CompletionStage[A]]
    ): TermHolder[JavaLanguage, Expression, Future[A]] =
      TermHolder[JavaLanguage, Expression, Future[A]](fa.value)

    override def toCompletionStage[From <: Expression, A](
        fa: TermHolder[JavaLanguage, From, Future[A]]
    ): TermHolder[JavaLanguage, Expression, CompletionStage[A]] =
      TermHolder[JavaLanguage, Expression, CompletionStage[A]](fa.value)

    // CompletableFuture.failedFuture() doesn't appear until Java 9, unfortunately.
    //
    // I considered creating an anonymous instance of CompletableFuture and using an in-line initializer
    // (aka "double-brace initialization"), which gives us the nicest-looking code, but that can create
    // odd memory leaks, unexpected GC pressure, and an unending stream of new java.lang.Class instances
    // that never get thrown out.
    //
    // Instead, even though it's kinda ugly and creates a little excess garbage, we make a lambda and then
    // immediately call it.
    override def failedFuture[From <: Expression, A, X <: Throwable](
        fx: TermHolder[JavaLanguage, From, X]
    )(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, Expression, Future[A]] = {
      val resultType                = typeFromClass(clsA.runtimeClass)
      val completionStageResultType = StaticJavaParser.parseClassOrInterfaceType("java.util.concurrent.CompletionStage").setTypeArguments(resultType)
      val supplierCompletionStageResultType =
        StaticJavaParser.parseClassOrInterfaceType("java.util.function.Supplier").setTypeArguments(completionStageResultType)
      val completableFutureType       = StaticJavaParser.parseClassOrInterfaceType("java.util.concurrent.CompletableFuture")
      val completableFutureResultType = completableFutureType.clone().setTypeArguments(resultType)

      TermHolder[JavaLanguage, Expression, Future[A]](
        new MethodCallExpr(
          new EnclosedExpr(
            new CastExpr(
              supplierCompletionStageResultType,
              new LambdaExpr(
                new NodeList[Parameter],
                new BlockStmt(
                  new NodeList(
                    new ExpressionStmt(
                      new VariableDeclarationExpr(
                        new VariableDeclarator(
                          completableFutureResultType,
                          "_failedFuture",
                          new ObjectCreationExpr(null, completableFutureType.setTypeArguments(), new NodeList)
                        ),
                        finalModifier
                      )
                    ),
                    new ExpressionStmt(
                      new MethodCallExpr(
                        new NameExpr("_failedFuture"),
                        "completeExceptionally",
                        new NodeList[Expression](fx.value)
                      )
                    ),
                    new ReturnStmt(new NameExpr("_failedFuture"))
                  )
                )
              )
            )
          ),
          "get"
        )
      )
    }

    override def onComplete[From <: Expression, A, X <: Throwable, Func <: Expression](
        fs: TermHolder[JavaLanguage, Func, A => Unit],
        ff: TermHolder[JavaLanguage, Func, X => Unit]
    )(fa: TermHolder[JavaLanguage, From, Future[A]]): TermHolder[JavaLanguage, MethodCallExpr, Unit] = {
      val (onSuccess, successParamName) = asBlock(fs.value, "result")
      val (onFailure, failureParamName) = asBlock(ff.value, "error")

      TermHolder[JavaLanguage, MethodCallExpr, Unit](
        new MethodCallExpr(
          fa.value,
          "whenComplete",
          new NodeList[Expression](
            new LambdaExpr(
              new NodeList(
                new Parameter(new UnknownType, successParamName),
                new Parameter(new UnknownType, failureParamName)
              ),
              new BlockStmt(
                new NodeList(
                  new IfStmt(
                    new BinaryExpr(new NameExpr(failureParamName), new NullLiteralExpr, BinaryExpr.Operator.NOT_EQUALS),
                    onFailure,
                    onSuccess
                  )
                )
              )
            )
          )
        )
      )
    }

    private def asBlock(expr: Expression, fallbackParamName: String): (BlockStmt, String) = expr match {
      case lambda: LambdaExpr =>
        lambda.getParameters.getFirst.asScala.fold(
          throw new IllegalStateException("LambdaExpr must have an argument")
        )(
          param =>
            (
              lambda.getBody match {
                case bs: BlockStmt      => bs
                case es: ExpressionStmt => new BlockStmt(new NodeList(es))
                case other =>
                  throw new IllegalStateException(
                    s"This shouldn't be possible: LambdaExpr contains a ${other.getClass}, but only BlockStmt and ExpressionStmt are valid"
                  )
              },
              param.getNameAsString
            )
        )
      case methRef: MethodReferenceExpr =>
        (
          new BlockStmt(
            new NodeList[Statement](
              new ExpressionStmt(
                new MethodCallExpr(
                  methRef.getScope,
                  methRef.getIdentifier,
                  new NodeList[Expression](new NameExpr(fallbackParamName))
                )
              )
            )
          ),
          fallbackParamName
        )
      case other =>
        (new BlockStmt(new NodeList(new ExpressionStmt(other))), fallbackParamName)
    }
  }

  private def typeFromClass(cls: Class[_]): Type = cls match {
    case java.lang.Boolean.TYPE   => PrimitiveType.booleanType.toBoxedType
    case java.lang.Byte.TYPE      => PrimitiveType.byteType.toBoxedType
    case java.lang.Character.TYPE => PrimitiveType.charType.toBoxedType
    case java.lang.Short.TYPE     => PrimitiveType.shortType.toBoxedType
    case java.lang.Integer.TYPE   => PrimitiveType.intType.toBoxedType
    case java.lang.Long.TYPE      => PrimitiveType.longType.toBoxedType
    case java.lang.Float.TYPE     => PrimitiveType.floatType.toBoxedType
    case java.lang.Double.TYPE    => PrimitiveType.doubleType.toBoxedType
    case other                    => StaticJavaParser.parseClassOrInterfaceType(other.getName)
  }

}

object JavaStdLibCollections extends JavaStdLibCollections
