package com.twilio.swagger.codegen

import _root_.io.swagger.models._
import _root_.io.swagger.models.parameters._
import _root_.io.swagger.models.properties._
import cats.syntax.either._
import com.twilio.swagger.codegen.extract.{Default, ScalaType}
import java.util.{Map => JMap}
import scala.collection.immutable.Seq
import scala.language.reflectiveCalls
import scala.meta._

object SwaggerUtil {
  def modelMetaType[T <: Model](model: T): Type = {
    model match {
      case ref: RefModel => Type.Name(ref.getSimpleRef())
      case arr: ArrayModel => t"List[${propMetaType(arr.getItems())}]"
      case impl: ModelImpl => typeName(impl.getType, Option(impl.getFormat), ScalaType(impl))
    }
  }

  def responseMetaType[T <: Response](response: T): Type = {
    response match {
      case r: RefResponse =>
        propMetaType(r.getSchema())
      case x =>
        println(s"responseMetaType: ${x} is being treated as Any")
        t"Any" // TODO: fill in rest
    }
  }

  case class ParamMeta(tpe: Type, defaultValue: Option[Term])
  def paramMeta[T <: Parameter](param: T): ParamMeta = {
    def getDefault[U <: AbstractSerializableParameter[U]: Default.GetDefault](p: U): Option[Term] = (
      Option(p.getType)
        .flatMap { _type =>
          val fmt = Option(p.getFormat)
          (_type, fmt) match {
            case ("string", None)           => Default(p).extract[String].map(Lit.String(_))
            case ("number", Some("float"))  => Default(p).extract[Float].map(Lit.Float(_))
            case ("number", Some("double")) => Default(p).extract[Double].map(Lit.Double(_))
            case ("integer", Some("int32")) => Default(p).extract[Int].map(Lit.Int(_))
            case ("integer", Some("int64")) => Default(p).extract[Long].map(Lit.Long(_))
            case ("boolean", None)          => Default(p).extract[Boolean].map(Lit.Boolean(_))
            case x                          => None
          }
        }
    )

    param match {
      case b: BodyParameter =>
        ParamMeta(modelMetaType(b.getSchema()), None)
      case h: HeaderParameter =>
        ParamMeta(typeName(h.getType(), Option(h.getFormat()), ScalaType(h)), getDefault(h))
      case p: PathParameter =>
        ParamMeta(typeName(p.getType(), Option(p.getFormat()), ScalaType(p)), getDefault(p))
      case q: QueryParameter =>
        ParamMeta(typeName(q.getType(), Option(q.getFormat()), ScalaType(q)), getDefault(q))
      case c: CookieParameter =>
        ParamMeta(typeName(c.getType(), Option(c.getFormat()), ScalaType(c)), getDefault(c))
      case f: FormParameter =>
        ParamMeta(typeName(f.getType(), Option(f.getFormat()), ScalaType(f)), getDefault(f))
      case r: RefParameter =>
        ParamMeta(Type.Name(r.getSimpleRef()), None)
      case s: SerializableParameter =>
        ParamMeta(typeName(s.getType(), Option(s.getFormat()), ScalaType(s)), None)
      case x =>
        println(s"paramMeta: ${x} is being treated as Any")
        ParamMeta(t"Any", None)
    }
  }

  // Standard type conversions, as documented in http://swagger.io/specification/#data-types-12
  def typeName(typeName: String, format: Option[String], customType: Option[String]): Type = {
    def log(fmt: Option[String], t: Type): Type = {
      fmt.foreach { fmt =>
        println(s"Warning: Deprecated behavior: Unsupported type '$fmt', falling back to $t. Please switch definitions to x-scala-type for custom types")
      }

      t
    }
    def liftCustomType(s: String): Option[Type] = {
      val terms = s.split('.').toList
      val (init, last) = (terms.init, terms.last)
      init.map(Term.Name.apply _) match {
        case Nil if last == "" => None
        case Nil => Some(Type.Name(last))
        case rest => Some(Type.Select(rest.reduceLeft(Term.Select.apply _), Type.Name(last)))
      }
    }

    customType.flatMap(liftCustomType _).getOrElse {
      (typeName, format) match {
        case ("string", Some("date-time"))  => t"java.time.OffsetDateTime"
        case ("string", o@Some(fmt))        => log(o, Type.Name(fmt))
        case ("string", None)               => log(None, t"String")
        case ("number", Some("float"))      => t"Float"
        case ("number", Some("double"))     => t"Double"
        case ("number", fmt)                => log(fmt, t"BigDecimal")
        case ("integer", Some("int32"))     => t"Int"
        case ("integer", Some("int64"))     => t"Long"
        case ("integer", fmt)               => log(fmt, t"BigInt")
        case ("boolean", fmt)               => log(fmt, t"Boolean")
        case ("array", fmt)                 => log(fmt, t"Iterable[String]")
        case ("file", o@Some(fmt))          => log(o, Type.Name(fmt))
        case ("file", fmt)                  => log(fmt, t"java.io.File")
        case ("object", fmt)                => log(fmt, t"Json")
        case (x, fmt)                       => {
          println(s"Fallback: ${x} (${fmt})")
          Type.Name(x)
        }
      }
    }
  }

  def escapeTree[T <: Tree]: T => T = _.transform({
    case Term.Name(name) => Term.Name(escapeReserved(name))
    case p@Term.Param(_, Term.Name(name), _, _) => p.copy(name=Term.Name(escapeReserved(name)))
    case Type.Name(name) => Type.Name(escapeReserved(name))
    case Ctor.Ref.Name(name) if name != "this" => Ctor.Ref.Name(escapeReserved(name)) // Literal "this" in ctor names is OK
  }).asInstanceOf[T]

  val unbacktick = "^`(.*)`$".r
  val leadingNumeric = "^[0-9\"]".r
  val invalidSymbols = "[-`\"'()\\.]".r
  val reservedWords = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
    "finally", "for", "forSome", "if", "implicit", "import", "lazy", "macro", "match", "new",
    "null", "object", "override", "package", "private", "protected", "return", "sealed", "super",
    "this", "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield",
    "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@"
  )

  def escapeReserved: String => String = {
    case name if unbacktick.findFirstMatchIn(name).nonEmpty => name
    case name if name.contains(' ') => name // scala.meta will automatically escape. See `EscapeTreeSpec.scala`
    case name if reservedWords.contains(name) => s"`${name}`"
    case name if invalidSymbols.findFirstMatchIn(name).nonEmpty => s"`${name}`"
    case name if leadingNumeric.findFirstMatchIn(name).nonEmpty => s"`${name}`"
    case name => name
  }

  def propMetaType[T <: Property](property: T): Type = {
    propMeta[T](property).tpe
  }

  case class PropMeta(tpe: Type, classDep: Option[Term.Name], defaultValue: Option[Term])
  def propMeta[T <: Property](property: T): PropMeta = {
    property match {
      case p: ArrayProperty =>
        val PropMeta(inner, dep, _) = propMeta(p.getItems)
        PropMeta(t"IndexedSeq[${inner}]", dep, None)
      case m: MapProperty =>
        val PropMeta(inner, dep, _) = propMeta(m.getAdditionalProperties)
        PropMeta(t"Map[String, ${inner}]", dep, None)
      case o: ObjectProperty =>
        PropMeta(t"Json", None, None) // TODO: o.getProperties
      case r: RefProperty =>
        PropMeta(Type.Name(r.getSimpleRef), Some(Term.Name(r.getSimpleRef)), None)

      case b: BooleanProperty =>
        PropMeta(t"Boolean", None, Default(b).extract[Boolean].map(Lit.Boolean(_)))
      case s: StringProperty =>
        PropMeta(typeName("string", Option(s.getFormat()), ScalaType(s)), None, Default(s).extract[String].map(Lit.String(_)))

      case d: DateProperty =>
        PropMeta(t"java.time.LocalDate", None, None)
      case d: DateTimeProperty =>
        PropMeta(t"java.time.OffsetDateTime", None, None)

      case l: LongProperty =>
        PropMeta(t"Long", None, Default(l).extract[Long].map(Lit.Long(_)))
      case i: IntegerProperty =>
        PropMeta(t"Int", None, Default(i).extract[Int].map(Lit.Int(_)))
      case f: FloatProperty =>
        PropMeta(t"Float", None, Default(f).extract[Float].map(Lit.Float(_)))
      case d: DoubleProperty =>
        PropMeta(t"Double", None, Default(d).extract[Double].map(Lit.Double(_)))
      case d: DecimalProperty =>
        PropMeta(t"BigDecimal", None, None)
      case p: AbstractProperty if p.getType.toLowerCase == "integer" =>
        PropMeta(t"BigInt", None, None)
      case p: AbstractProperty if p.getType.toLowerCase == "number" =>
        PropMeta(t"BigDecimal", None, None)
      case p: AbstractProperty if p.getType.toLowerCase == "string" =>
        PropMeta(t"String", None, None)
      case x =>
        println(s"propMeta: ${x} is being treated as Any")
        PropMeta(t"Any", None, None)
    }
  }

  /*
    Required \ Default  || Defined  || Undefined / NULL ||
    =====================================================
    TRUE                || a: T = v || a: T             ||
    FALSE / NULL        || a: T = v || a: Opt[T] = None ||
  */

  private[this] val successCodesWithEntities = Seq(200, 201, 202, 203, 206, 226).map(_.toString)
  private[this] val successCodesWithoutEntities = Seq(204, 205).map(_.toString)

  private[this] def getBestSuccessResponse(responses: JMap[String, Response]): Option[Response] =
    successCodesWithEntities.find(responses.containsKey).flatMap(code => Option(responses.get(code)))
  private[this] def hasEmptySuccessType(responses: JMap[String, Response]): Boolean =
    successCodesWithoutEntities.exists(responses.containsKey)

  def getResponseType(httpMethod: HttpMethod, operation: Operation, ignoredType: Type = t"IgnoredEntity"): Type = {
    if (httpMethod == HttpMethod.GET || httpMethod == HttpMethod.PUT || httpMethod == HttpMethod.POST) {
      Option(operation.getResponses).flatMap { responses =>
        getBestSuccessResponse(responses)
          .flatMap(resp => Option(resp.getSchema))
          .map(SwaggerUtil.propMetaType)
          .orElse(if (hasEmptySuccessType(responses)) Some(ignoredType) else None)
      }.getOrElse(ignoredType)
    } else {
      ignoredType
    }
  }

  object paths {
    def generateUrlPathParams(path: String)(termMunger: Term.Name => Term.Name): Target[Term] = {
      import atto._, Atto._

      val term: Parser[Term.Apply] = many(letter).map(_.mkString("")).map(term => q"Formatter.addPath(${termMunger(Term.Name(term))})")
      val variable: Parser[Term.Apply] = char('{') ~> term <~ char('}')
      val other: Parser[String] = many1(notChar('{')).map(_.toList.mkString)
      val pattern: Parser[List[Either[String, Term.Apply]]] = many(either(variable, other).map(_.swap: Either[String, Term.Apply]))

      for {
        parts <- pattern.parseOnly(path).either.fold(Target.log(_), Target.pure(_))
        result = parts.map({
          case Left(part) => Lit.String(part)
          case Right(term) => term
        }).foldLeft[Term](q"host + basePath")({ case (a, b) => q"${a} + ${b}" })
      } yield result
    }
  }
}
