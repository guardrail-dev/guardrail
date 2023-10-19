package core.issues

import scala.language.{ higherKinds, reflectiveCalls }

import cats.effect.IO
import cats.data.{ Kleisli, ValidatedNel }
import org.http4s._
import org.http4s.client.{ Client => Http4sClient }
import org.http4s.blaze.client._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.multipart._
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import io.circe._
import org.scalatest.enablers.Aggregating
import org.scalactic.Equality
import org.http4s.dsl.impl.{ OptionalMultiQueryParamDecoderMatcher, QueryParamDecoderMatcher }

class Issue1218Suite extends AnyFunSuite with Matchers with EitherValues with ScalaFutures {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 1 second)

  // Hackily provide Aggregating instance for Option
  implicit def aggregatingOption[A](implicit ev: Equality[A]): Aggregating[Option[A]] = new Aggregating[Option[A]] {
    val proxy = Aggregating.aggregatingNatureOfGenTraversable[A, Iterable](ev)

    def containsAllOf(aggregation: Option[A], eles: _root_.scala.collection.Seq[Any]): Boolean        = proxy.containsAllOf(aggregation, eles)
    def containsAtLeastOneOf(aggregation: Option[A], eles: _root_.scala.collection.Seq[Any]): Boolean = proxy.containsAtLeastOneOf(aggregation, eles)
    def containsAtMostOneOf(aggregation: Option[A], eles: _root_.scala.collection.Seq[Any]): Boolean  = proxy.containsAtMostOneOf(aggregation, eles)
    def containsOnly(aggregation: Option[A], eles: _root_.scala.collection.Seq[Any]): Boolean         = proxy.containsOnly(aggregation, eles)
    def containsTheSameElementsAs(leftAggregation: Option[A], rightAggregation: _root_.scala.collection.GenTraversable[Any]): Boolean =
      proxy.containsTheSameElementsAs(leftAggregation, rightAggregation)
  }

  class CompareQueryParamDecoderMatcherHolder[A, Container[_]](val dummy: Boolean = true) {
    def apply[
        Ours <: { def unapply(params: Map[String, _root_.scala.collection.Seq[String]]): Option[Container[A]] }
    ](
        ours: Ours,
        key: String
    )(implicit ev1: Equality[A], ev2: Aggregating[Container[A]], ev3: QueryParamDecoder[A]): Map[String, _root_.scala.collection.Seq[String]] => Unit = {
      cases =>
        val theirs = new QueryParamDecoderMatcher[A](key) {}

        cases.foreach { case (label, values) =>
          val params = Map(key -> values)
          ours.unapply(params) should contain theSameElementsAs (theirs.unapplySeq(params))
        }
    }
  }
  def compareQPDM[A, Container[_]]: CompareQueryParamDecoderMatcherHolder[A, Container] = new CompareQueryParamDecoderMatcherHolder[A, Container]()

  class CompareOptionalMultiQueryParamDecoderMatcherHolder[A, Container[_]](val dummy: Boolean = true) {
    def apply[
        Ours <: { def unapply(params: Map[String, collection.Seq[String]]): Option[Option[Container[A]]] }
    ](ours: Ours, key: String)(implicit ev1: Equality[A], ev2: Aggregating[Container[A]], ev3: QueryParamDecoder[A]): Map[String, Seq[String]] => Unit = {
      cases =>
        val theirs = new OptionalMultiQueryParamDecoderMatcher[A](key) {}

        cases.foreach { case (label, values) =>
          val params = Map(key -> values)
          ours.unapply(params) should contain theSameElementsAs theirs
            .unapply(params)
            .collectFirst { case cats.data.Validated.Valid(value) =>
              Option(value).filter(_.nonEmpty)
            }
        }
    }
  }
  def compareOMQPDM[A, Container[_]]: CompareOptionalMultiQueryParamDecoderMatcherHolder[A, Container] =
    new CompareOptionalMultiQueryParamDecoderMatcherHolder[A, Container]()

  test("All query parameter matchers behave the same as in-built ones") {
    val resource = new issues.issue1218.server.http4s.Resource[IO]()

    val stringCases = Map[String, Seq[String]](
      "empty"     -> Seq.empty,
      "single"    -> Seq("foo"),
      "double"    -> Seq("foo", "bar"),
      "repeating" -> Seq("foo", "foo")
    )

    val longCases = Map[String, Seq[String]](
      "empty"     -> Seq.empty,
      "single"    -> Seq("123"),
      "double"    -> Seq("123", "234"),
      "repeating" -> Seq("123", "123")
    )

    compareOMQPDM[Long, Iterable](resource.DoFooOptrefidxseqMatcher, "optrefidxseq").apply(longCases)
    compareOMQPDM[Long, List](resource.DoFooOptreflistMatcher, "optreflist").apply(longCases)
    compareOMQPDM[Long, Seq](resource.DoFooOptrefseqMatcher, "optrefseq").apply(longCases)
    compareOMQPDM[Long, Vector](resource.DoFooOptrefvecMatcher, "optrefvec").apply(longCases)
    compareOMQPDM[Long, Iterable](resource.DoFooOptidxseqMatcher, "optidxseq").apply(stringCases)
    compareOMQPDM[Long, Iterable](resource.DoFooOptlistMatcher, "optlist").apply(stringCases)
    compareOMQPDM[Long, Iterable](resource.DoFooOptseqMatcher, "optseq").apply(stringCases)
    compareOMQPDM[Long, Iterable](resource.DoFooOptvectorMatcher, "optvector").apply(stringCases)
    compareQPDM[Long, IndexedSeq](resource.DoFooRefidxseqMatcher, "refidxseq").apply(longCases)
    compareQPDM[Long, List](resource.DoFooReflistMatcher, "reflist").apply(longCases)
    compareQPDM[Long, Seq](resource.DoFooRefseqMatcher, "refseq").apply(longCases)
    compareQPDM[Long, Vector](resource.DoFooRefvecMatcher, "refvec").apply(longCases)
    compareQPDM[Long, Iterable](resource.DoFooIdxseqMatcher, "idxseq").apply(stringCases)
    compareQPDM[Long, Iterable](resource.DoFooListMatcher, "list").apply(stringCases)
    compareQPDM[Long, Iterable](resource.DoFooSeqMatcher, "seq").apply(stringCases)
    compareQPDM[Long, Iterable](resource.DoFooVectorMatcher, "vector").apply(stringCases)
  }
}
