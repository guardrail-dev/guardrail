package dev.guardrail.terms.collections

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.Modifier.finalModifier
import com.github.javaparser.ast.`type`.{ ArrayType, Type, UnknownType }
import com.github.javaparser.ast.body.{ Parameter, VariableDeclarator }
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.{ ArrayCreationLevel, NodeList }
import dev.guardrail.languages.JavaLanguage
import dev.guardrail.terms.collections.JavaCollectionsHelpers._
import dev.guardrail.terms.collections.JavaStdLibCollectionsHelpers.JavaStdLibTermHolder

import java.util.concurrent.CompletionStage
import scala.compat.java8.OptionConverters._
import scala.concurrent.Future
import scala.reflect.ClassTag

object JavaStdLibCollectionsHelpers {
  // This exists because the Java stdlib collections do not have operations like map and flatMap on
  // the collections class itself.  You must call stream() first, and then when you want a collection
  // back, call collect().  What we want to do is call stream() the first time a "monadic" operation is
  // called, and not call it again until we want to pull the collection "out" of the term holder.  So
  // _value will be the Stream instance, and value will return a collected List instance.
  case class JavaStdLibTermHolder[HeldType](_value: MethodCallExpr) extends TermHolder[JavaLanguage, MethodCallExpr, HeldType](_value) {
    override lazy val value: MethodCallExpr = doCollect(_value)
  }
}

@SuppressWarnings(Array("org.wartremover.warts.Null", "org.wartremover.warts.Throw"))
trait JavaStdLibCollections extends CollectionsAbstraction[JavaLanguage] {
  override implicit val optionInstances: OptionF[JavaLanguage] = new OptionF[JavaLanguage] {
    override def liftType(tpe: Type): Type  = StaticJavaParser.parseClassOrInterfaceType("java.util.Optional").setTypeArguments(tpe)
    override def isType(tpe: Type): Boolean = isContainerOfType(tpe, "java.util", "Optional")

    override def pure[From <: Expression, A](fa: TermHolder[JavaLanguage, From, A]): TermHolder[JavaLanguage, MethodCallExpr, Option[A]] =
      doMethodCall(new NameExpr("java.util.Optional"), "ofNullable", fa.value)

    override def empty[A]: TermHolder[JavaLanguage, MethodCallExpr, Option[A]] =
      TermHolder[JavaLanguage, MethodCallExpr, Option[A]](new MethodCallExpr(new NameExpr("java.util.Optional"), "empty"))

    override def filter[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Boolean]
    )(fa: TermHolder[JavaLanguage, From, Option[A]])(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, MethodCallExpr, Option[A]] =
      doMethodCall(fa.value, "filter", f.value)

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

  override implicit val listInstances: ListF[JavaLanguage] = new ListF[JavaLanguage] {
    override def liftType(tpe: Type): Type = StaticJavaParser.parseClassOrInterfaceType("java.util.List").setTypeArguments(tpe)
    override def isType(tpe: Type): Boolean =
      isContainerOfType(tpe, "java.util", "List") ||
        isContainerOfType(tpe, "java.util", "List")

    override def pure[From <: Expression, A](fa: TermHolder[JavaLanguage, From, A]): TermHolder[JavaLanguage, MethodCallExpr, List[A]] =
      TermHolder[JavaLanguage, MethodCallExpr, List[A]](
        new MethodCallExpr(
          new NameExpr("java.util.Collections"),
          "singletonList",
          new NodeList[Expression](fa.value)
        )
      )

    override def empty[A]: TermHolder[JavaLanguage, MethodCallExpr, List[A]] =
      TermHolder[JavaLanguage, MethodCallExpr, List[A]](
        new MethodCallExpr(new NameExpr("java.util.Collections"), "emptyList")
      )

    override def filter[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Boolean]
    )(fa: TermHolder[JavaLanguage, From, List[A]])(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, MethodCallExpr, List[A]] =
      JavaStdLibTermHolder[List[A]](
        new MethodCallExpr(
          // If we already are a Stream, use it as-is.  Otherwise call stream()
          fa match {
            case jslth: JavaStdLibTermHolder[_] => jslth._value
            case th                             => wrapStream(th.value)
          },
          "filter",
          new NodeList[Expression](f.value)
        )
      )

    override def foreach[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Unit]
    )(fa: TermHolder[JavaLanguage, From, List[A]]): TermHolder[JavaLanguage, MethodCallExpr, Unit] =
      TermHolder[JavaLanguage, MethodCallExpr, Unit](
        new MethodCallExpr(
          // forEach() exists on both Stream and List; if we're already a Stream, stick with the Stream,
          // as converting to a List is unnecessary and expensive.
          fa match {
            case jslth: JavaStdLibTermHolder[_] => jslth._value
            case th                             => th.value
          },
          "forEach",
          new NodeList[Expression](f.value)
        )
      )

    override def map[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => B]
    )(fa: TermHolder[JavaLanguage, From, List[A]]): TermHolder[JavaLanguage, MethodCallExpr, List[B]] =
      JavaStdLibTermHolder[List[B]](
        new MethodCallExpr(
          // If we already are a Stream, use it as-is.  Otherwise call stream()
          fa match {
            case jslth: JavaStdLibTermHolder[_] => jslth._value
            case th                             => wrapStream(th.value)
          },
          "map",
          new NodeList[Expression](f.value)
        )
      )

    override def flatMap[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => List[B]]
    )(fa: TermHolder[JavaLanguage, From, List[A]]): TermHolder[JavaLanguage, MethodCallExpr, List[B]] =
      JavaStdLibTermHolder[List[B]](
        new MethodCallExpr(
          // If we already are a Stream, use it as-is.  Otherwise call stream()
          fa match {
            case jslth: JavaStdLibTermHolder[_] => jslth._value
            case th                             => wrapStream(th.value)
          },
          "flatMap",
          new NodeList[Expression](f.value)
        )
      )

    override def toArray[From <: Expression, A](
        fa: TermHolder[JavaLanguage, From, List[A]]
    )(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, MethodCallExpr, Array[A]] =
      TermHolder[JavaLanguage, MethodCallExpr, Array[A]](
        fa match {
          case jslth: JavaStdLibTermHolder[_] =>
            val resultType = typeFromClass(clsA.runtimeClass)
            new MethodCallExpr(
              jslth._value,
              "toArray",
              new NodeList[Expression](new MethodReferenceExpr(new TypeExpr(new ArrayType(resultType)), null, "new"))
            )
          case th =>
            val resultType = typeFromClass(clsA.runtimeClass, boxPrimitives = false)
            new MethodCallExpr(
              th.value,
              "toArray",
              new NodeList[Expression](new ArrayCreationExpr(resultType, new NodeList(new ArrayCreationLevel(new IntegerLiteralExpr("0"))), null))
            )
        }
      )
  }

  override implicit val futureInstances: FutureF[JavaLanguage] = new FutureF[JavaLanguage] {
    override def liftType(tpe: Type): Type = StaticJavaParser.parseClassOrInterfaceType("java.util.concurrent.CompletionStage").setTypeArguments(tpe)
    override def isType(tpe: Type): Boolean =
      isContainerOfType(tpe, "java.util.concurrent", "CompletionStage") ||
        isContainerOfType(tpe, "java.util.concurrent", "CompletableFuture")

    override def pure[From <: Expression, A](fa: TermHolder[JavaLanguage, From, A]): TermHolder[JavaLanguage, MethodCallExpr, Future[A]] =
      doMethodCall(new NameExpr("java.util.concurrent.CompletableFuture"), "completedFuture", fa.value)

    override def filter[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Boolean]
    )(fa: TermHolder[JavaLanguage, From, Future[A]])(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, MethodCallExpr, Future[A]] = {
      val resultType                 = typeFromClass(clsA.runtimeClass)
      val predicateResultType        = StaticJavaParser.parseClassOrInterfaceType("java.util.function.Predicate").setTypeArguments(resultType)
      val noSuchElementExceptionType = StaticJavaParser.parseClassOrInterfaceType("NoSuchElementException")
      TermHolder[JavaLanguage, MethodCallExpr, Future[A]](
        new MethodCallExpr(
          fa.value,
          "thenCompose",
          new NodeList[Expression](
            new LambdaExpr(
              new Parameter(new UnknownType, "_result"),
              new BlockStmt(
                new NodeList(
                  new IfStmt(
                    new MethodCallExpr(
                      new EnclosedExpr(new CastExpr(predicateResultType, f.value)),
                      "test",
                      new NodeList[Expression](new NameExpr("_result"))
                    ),
                    new BlockStmt(
                      new NodeList(
                        new ReturnStmt(pure(TermHolder[JavaLanguage, NameExpr, A](new NameExpr("_result"))).value)
                      )
                    ),
                    new BlockStmt(
                      new NodeList(
                        new ReturnStmt(
                          failedFuture(
                            TermHolder[JavaLanguage, ObjectCreationExpr, NoSuchElementException](
                              new ObjectCreationExpr(
                                null,
                                noSuchElementExceptionType,
                                new NodeList[Expression](new StringLiteralExpr("Filter predicate did not match"))
                              )
                            )
                          ).value
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    }

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
}

object JavaStdLibCollections extends JavaStdLibCollections
