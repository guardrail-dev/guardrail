package dev.guardrail.terms.collections

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.{ ArrayCreationLevel, NodeList }
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.expr.{ ArrayCreationExpr, Expression, IntegerLiteralExpr, MethodCallExpr, NameExpr }
import dev.guardrail.languages.JavaLanguage
import dev.guardrail.terms.collections.JavaCollectionsHelpers.{ doMethodCall, isContainerOfType, typeFromClass }

import java.util.concurrent.CompletionStage
import scala.concurrent.Future
import scala.reflect.ClassTag

@SuppressWarnings(Array("org.wartremover.warts.Null"))
trait JavaVavrCollections extends CollectionsAbstraction[JavaLanguage] {
  override implicit val optionInstances: OptionF[JavaLanguage] = new OptionF[JavaLanguage] {
    override def liftType(tpe: Type): Type  = StaticJavaParser.parseClassOrInterfaceType("io.vavr.control.Option").setTypeArguments(tpe)
    override def isType(tpe: Type): Boolean = isContainerOfType(tpe, "io.vavr.control", "Option")

    override def pure[From <: Expression, A](fa: TermHolder[JavaLanguage, From, A]): TermHolder[JavaLanguage, MethodCallExpr, Option[A]] =
      doMethodCall(new NameExpr("io.vavr.control.Option"), "of", fa.value)

    override def empty[A]: TermHolder[JavaLanguage, MethodCallExpr, Option[A]] =
      TermHolder[JavaLanguage, MethodCallExpr, Option[A]](new MethodCallExpr(new NameExpr("io.vavr.control.Option"), "none"))

    override def foreach[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Unit]
    )(fa: TermHolder[JavaLanguage, From, Option[A]]): TermHolder[JavaLanguage, MethodCallExpr, Unit] =
      doMethodCall(fa.value, "forEach", f.value)

    override def filter[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Boolean]
    )(fa: TermHolder[JavaLanguage, From, Option[A]])(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, MethodCallExpr, Option[A]] =
      doMethodCall(fa.value, "filter", f.value)

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
      doMethodCall(fa.value, "getOrElse", f.value)

    override def getOrElseNull[From <: Expression, A](fa: TermHolder[JavaLanguage, From, Option[A]]): TermHolder[JavaLanguage, MethodCallExpr, A] =
      TermHolder[JavaLanguage, MethodCallExpr, A](new MethodCallExpr(fa.value, "getOrNull"))

    override def getOrElseThrow[From <: Expression, A, X <: Throwable, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, () => X]
    )(fa: TermHolder[JavaLanguage, From, Option[A]]): TermHolder[JavaLanguage, MethodCallExpr, A] =
      doMethodCall(fa.value, "getOrElseThrow", f.value)
  }

  override implicit val listInstances: ListF[JavaLanguage] = new ListF[JavaLanguage] {
    override def liftType(tpe: Type): Type = StaticJavaParser.parseClassOrInterfaceType("io.vavr.collection.Vector").setTypeArguments(tpe)

    override def isType(tpe: Type): Boolean =
      isContainerOfType(tpe, "io.vavr.collection", "Vector") ||
        isContainerOfType(tpe, "io.vavr.collection", "List") ||
        isContainerOfType(tpe, "io.vavr.collection", "Seq")

    override def pure[From <: Expression, A](fa: TermHolder[JavaLanguage, From, A]): TermHolder[JavaLanguage, MethodCallExpr, List[A]] =
      TermHolder[JavaLanguage, MethodCallExpr, List[A]](
        new MethodCallExpr(
          new NameExpr("io.vavr.collection.Vector"),
          "of",
          new NodeList[Expression](fa.value)
        )
      )

    override def empty[A]: TermHolder[JavaLanguage, MethodCallExpr, List[A]] =
      TermHolder[JavaLanguage, MethodCallExpr, List[A]](
        new MethodCallExpr(new NameExpr("List"), "empty")
      )

    override def foreach[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Unit]
    )(fa: TermHolder[JavaLanguage, From, List[A]]): TermHolder[JavaLanguage, MethodCallExpr, Unit] =
      TermHolder[JavaLanguage, MethodCallExpr, Unit](
        new MethodCallExpr(
          fa.value,
          "forEach",
          new NodeList[Expression](f.value)
        )
      )

    override def filter[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Boolean]
    )(fa: TermHolder[JavaLanguage, From, List[A]])(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, MethodCallExpr, List[A]] =
      doMethodCall(fa.value, "filter", f.value)

    override def map[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => B]
    )(fa: TermHolder[JavaLanguage, From, List[A]]): TermHolder[JavaLanguage, MethodCallExpr, List[B]] =
      TermHolder[JavaLanguage, MethodCallExpr, List[B]](
        new MethodCallExpr(
          fa.value,
          "map",
          new NodeList[Expression](f.value)
        )
      )

    override def flatMap[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => List[B]]
    )(fa: TermHolder[JavaLanguage, From, List[A]]): TermHolder[JavaLanguage, MethodCallExpr, List[B]] =
      TermHolder[JavaLanguage, MethodCallExpr, List[B]](
        new MethodCallExpr(
          fa.value,
          "flatMap",
          new NodeList[Expression](f.value)
        )
      )

    override def toArray[From <: Expression, A](
        fa: TermHolder[JavaLanguage, From, List[A]]
    )(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, MethodCallExpr, Array[A]] = {
      val resultType = typeFromClass(clsA.runtimeClass, boxPrimitives = false)
      TermHolder[JavaLanguage, MethodCallExpr, Array[A]](
        new MethodCallExpr(
          new MethodCallExpr(
            fa.value,
            "asJava"
          ),
          "toArray",
          new NodeList[Expression](new ArrayCreationExpr(resultType, new NodeList(new ArrayCreationLevel(new IntegerLiteralExpr("0"))), null))
        )
      )
    }
  }

  override implicit val futureInstances: FutureF[JavaLanguage] = new FutureF[JavaLanguage] {
    override def liftType(tpe: Type): Type  = StaticJavaParser.parseClassOrInterfaceType("io.vavr.concurrent.Future").setTypeArguments(tpe)
    override def isType(tpe: Type): Boolean = isContainerOfType(tpe, "io.vavr.concurrent", "Future")

    override def pure[From <: Expression, A](fa: TermHolder[JavaLanguage, From, A]): TermHolder[JavaLanguage, MethodCallExpr, Future[A]] =
      doMethodCall(new NameExpr("io.vavr.concurrent.Future"), "successful", fa.value)

    override def foreach[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Unit]
    )(fa: TermHolder[JavaLanguage, From, Future[A]]): TermHolder[JavaLanguage, MethodCallExpr, Unit] =
      doMethodCall(fa.value, "onSuccess", f.value)

    override def filter[From <: Expression, A, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Boolean]
    )(fa: TermHolder[JavaLanguage, From, Future[A]])(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, MethodCallExpr, Future[A]] =
      doMethodCall(fa.value, "filter", f.value)

    override def map[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => B]
    )(fa: TermHolder[JavaLanguage, From, Future[A]]): TermHolder[JavaLanguage, MethodCallExpr, Future[B]] =
      doMethodCall(fa.value, "map", f.value)

    override def flatMap[From <: Expression, A, B, Func <: Expression](
        f: TermHolder[JavaLanguage, Func, A => Future[B]]
    )(fa: TermHolder[JavaLanguage, From, Future[A]]): TermHolder[JavaLanguage, MethodCallExpr, Future[B]] =
      doMethodCall(fa.value, "flatMap", f.value)

    override def fromCompletionStage[From <: Expression, A](
        fa: TermHolder[JavaLanguage, From, CompletionStage[A]]
    ): TermHolder[JavaLanguage, Expression, Future[A]] =
      doMethodCall(
        new NameExpr("io.vavr.concurrent.Future"),
        "fromCompletableFuture",
        new MethodCallExpr(fa.value, "toCompletableFuture")
      )

    override def toCompletionStage[From <: Expression, A](
        fa: TermHolder[JavaLanguage, From, Future[A]]
    ): TermHolder[JavaLanguage, Expression, CompletionStage[A]] =
      TermHolder[JavaLanguage, Expression, CompletionStage[A]](new MethodCallExpr(fa.value, "toCompletableFuture"))

    override def failedFuture[From <: Expression, A, X <: Throwable](
        fx: TermHolder[JavaLanguage, From, X]
    )(implicit clsA: ClassTag[A]): TermHolder[JavaLanguage, Expression, Future[A]] =
      doMethodCall(new NameExpr("io.vavr.concurrent.Future"), "failed", fx.value)

    override def onComplete[From <: Expression, A, X <: Throwable, Func <: Expression](
        fs: TermHolder[JavaLanguage, Func, A => Unit],
        ff: TermHolder[JavaLanguage, Func, X => Unit]
    )(fa: TermHolder[JavaLanguage, From, Future[A]]): TermHolder[JavaLanguage, MethodCallExpr, Unit] =
      TermHolder[JavaLanguage, MethodCallExpr, Unit](
        new MethodCallExpr(
          new MethodCallExpr(
            fa.value,
            "onSuccess",
            new NodeList[Expression](fs.value)
          ),
          "onFailure",
          new NodeList[Expression](ff.value)
        )
      )
  }
}

object JavaVavrCollections extends JavaVavrCollections
