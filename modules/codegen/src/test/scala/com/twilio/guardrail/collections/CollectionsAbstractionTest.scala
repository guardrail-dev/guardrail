package com.twilio.guardrail.collections

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ PrimitiveType, UnknownType }
import com.github.javaparser.ast.body.Parameter
import com.github.javaparser.ast.expr._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.terms.collections._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.concurrent.Future

class CollectionsAbstractionTest extends AnyFreeSpec with Matchers {
  def optionalPipeline(implicit Ca: CollectionsAbstraction[JavaLanguage]): TermHolder[JavaLanguage, MethodCallExpr, Int] = {
    import Ca._

    val a = new IntegerLiteralExpr("5").lift[Int]

    val fa = a.liftOptional

    val f = new LambdaExpr(
      new Parameter(new UnknownType, "a"),
      new ConditionalExpr(
        new BinaryExpr(new NameExpr("a"), new IntegerLiteralExpr("0"), BinaryExpr.Operator.GREATER_EQUALS),
        new NameExpr("a").lift[Int].liftOptional.value,
        emptyOptional.value
      )
    ).lift[Int => Option[Int]]

    val f2 = new LambdaExpr(
      new Parameter(new UnknownType, "a"),
      new BinaryExpr(new NameExpr("a"), new IntegerLiteralExpr("1"), BinaryExpr.Operator.PLUS)
    ).lift[Int => Int]

    val f3 = new LambdaExpr(new NodeList[Parameter], new IntegerLiteralExpr("42")).lift[() => Int]

    fa.flatMap(f).map(f2).getOrElse(f3)
  }

  def vectorPipeline(implicit Ca: CollectionsAbstraction[JavaLanguage]): TermHolder[JavaLanguage, MethodCallExpr, Vector[Int]] = {
    import Ca._

    val a = new IntegerLiteralExpr("5").lift[Int]

    val fa = a.liftVector

    val f = new LambdaExpr(
      new Parameter(new UnknownType, "a"),
      new BinaryExpr(new NameExpr("a"), new IntegerLiteralExpr("1"), BinaryExpr.Operator.PLUS)
    ).lift[Int => Int]

    fa.map(f)
  }

  def futurePipeline(implicit Ca: CollectionsAbstraction[JavaLanguage]): TermHolder[JavaLanguage, MethodCallExpr, Future[Int]] = {
    import Ca._

    val a = new IntegerLiteralExpr("5").lift[Int]

    val fa = a.liftFuture

    val f = new LambdaExpr(
      new Parameter(new UnknownType, "a"),
      new ConditionalExpr(
        new BinaryExpr(new NameExpr("a"), new IntegerLiteralExpr("0"), BinaryExpr.Operator.GREATER_EQUALS),
        new NameExpr("a").lift[Int].liftFuture.value,
        new ObjectCreationExpr(
          null,
          StaticJavaParser.parseClassOrInterfaceType("IllegalStateException"),
          new NodeList[Expression](new StringLiteralExpr("Negative!"))
        ).lift[IllegalStateException].toFailedFuture[Int].value
      )
    ).lift[Int => Future[Int]]

    fa.flatMap(f)
  }

  def failedFuture(implicit Ca: CollectionsAbstraction[JavaLanguage]): TermHolder[JavaLanguage, Expression, Future[Int]] = {
    import Ca._
    val exceptionType = StaticJavaParser.parseClassOrInterfaceType("Exception")
    new ObjectCreationExpr(null, exceptionType, new NodeList).lift[Exception].toFailedFuture[Int]
  }

  "Collections abstraction over Java stdlib" - {
    import com.twilio.guardrail.terms.collections.JavaStdLibCollections
    implicit val Ca = JavaStdLibCollections

    "Type tests should work" in {
      import Ca._

      val optionalType  = StaticJavaParser.parseClassOrInterfaceType("java.util.Optional").setTypeArguments(PrimitiveType.intType.toBoxedType)
      val vectorType    = StaticJavaParser.parseClassOrInterfaceType("java.util.List").setTypeArguments(PrimitiveType.intType.toBoxedType)
      val futureType    = StaticJavaParser.parseClassOrInterfaceType("java.util.concurrent.CompletionStage").setTypeArguments(PrimitiveType.intType.toBoxedType)
      val exceptionType = StaticJavaParser.parseClassOrInterfaceType("Exception")

      optionalType.isOptionalType mustBe true
      vectorType.isVectorType mustBe true
      futureType.isFutureType mustBe true

      exceptionType.isOptionalType mustBe false
      exceptionType.isVectorType mustBe false
      exceptionType.isFutureType mustBe false
    }

    "Optional pipelines should render" in {
      optionalPipeline.value.toString mustBe "java.util.Optional.ofNullable(5).flatMap(a -> a >= 0 ? java.util.Optional.ofNullable(a) : java.util.Optional.empty()).map(a -> a + 1).orElseGet(() -> 42)"
    }

    "Vector pipelines should render" in {
      vectorPipeline.value.toString mustBe "java.util.Collections.singletonList(5).stream().map(a -> a + 1).collect(java.util.stream.Collectors.toList())"
    }

    "Future pipelines should render" in {
      futurePipeline.value.toString mustBe
        """java.util.concurrent.CompletableFuture.completedFuture(5).thenCompose(a -> a >= 0 ? java.util.concurrent.CompletableFuture.completedFuture(a) : ((java.util.function.Supplier<java.util.concurrent.CompletionStage<Integer>>) () -> {
        |    final java.util.concurrent.CompletableFuture<Integer> _failedFuture = new java.util.concurrent.CompletableFuture<>();
        |    _failedFuture.completeExceptionally(new IllegalStateException("Negative!"));
        |    return _failedFuture;
        |}).get())""".stripMargin
    }

    "Failed future should render" in {
      failedFuture.value.toString mustBe """((java.util.function.Supplier<java.util.concurrent.CompletionStage<Integer>>) () -> {
        |    final java.util.concurrent.CompletableFuture<Integer> _failedFuture = new java.util.concurrent.CompletableFuture<>();
        |    _failedFuture.completeExceptionally(new Exception());
        |    return _failedFuture;
        |}).get()""".stripMargin
    }
  }

  "Collections abstraction over Java Vavr" - {
    import com.twilio.guardrail.terms.collections.JavaVavrCollections
    implicit val Ca = JavaVavrCollections

    "Type tests should work" in {
      import Ca._

      val optionalType  = StaticJavaParser.parseClassOrInterfaceType("io.vavr.control.Option").setTypeArguments(PrimitiveType.intType.toBoxedType)
      val vectorType    = StaticJavaParser.parseClassOrInterfaceType("io.vavr.collection.Vector").setTypeArguments(PrimitiveType.intType.toBoxedType)
      val futureType    = StaticJavaParser.parseClassOrInterfaceType("io.vavr.concurrent.Future").setTypeArguments(PrimitiveType.intType.toBoxedType)
      val exceptionType = StaticJavaParser.parseClassOrInterfaceType("Exception")

      optionalType.isOptionalType mustBe true
      vectorType.isVectorType mustBe true
      futureType.isFutureType mustBe true

      exceptionType.isOptionalType mustBe false
      exceptionType.isVectorType mustBe false
      exceptionType.isFutureType mustBe false
    }

    "Optional pipelines should render" in {
      optionalPipeline.value.toString mustBe "io.vavr.control.Option.of(5).flatMap(a -> a >= 0 ? io.vavr.control.Option.of(a) : io.vavr.control.Option.none()).map(a -> a + 1).getOrElse(() -> 42)"
    }

    "Vector pipelines should render" in {
      vectorPipeline.value.toString mustBe "io.vavr.collection.Vector.of(5).map(a -> a + 1)"
    }

    "Future pipelines should render" in {
      futurePipeline.value.toString mustBe """io.vavr.concurrent.Future.successful(5).flatMap(a -> a >= 0 ? io.vavr.concurrent.Future.successful(a) : io.vavr.concurrent.Future.failed(new IllegalStateException("Negative!")))"""
    }

    "Failed future should render" in {
      failedFuture.value.toString mustBe "io.vavr.concurrent.Future.failed(new Exception())"
    }
  }
}
