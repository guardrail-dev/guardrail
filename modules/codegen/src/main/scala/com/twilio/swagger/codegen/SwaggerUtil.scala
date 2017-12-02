package com.twilio.swagger.codegen

import _root_.io.swagger.models._
import _root_.io.swagger.models.parameters._
import _root_.io.swagger.models.properties._
import cats.syntax.either._
import com.twilio.swagger.codegen.extract.{Default, ScalaType}
import com.twilio.swagger.codegen.generators.ScalaParameter
import java.util.{Map => JMap}
import scala.language.reflectiveCalls
import scala.meta._

object SwaggerUtil {
  def modelMetaType[T <: Model](model: T): Target[Type] = {
    model match {
      case ref: RefModel =>
        for {
          ref <- Target.fromOption(Option(ref.getSimpleRef()), "Unspecified $ref")
        } yield Type.Name(ref)
      case arr: ArrayModel =>
        for {
          items <- Target.fromOption(Option(arr.getItems()), "items.type unspecified")
          meta <- propMeta(items)
        } yield t"List[${meta.tpe}]"
      case impl: ModelImpl =>
        for {
          tpeName <- Target.fromOption(Option(impl.getType()), s"Unable to resolve type for ${impl}")
        } yield typeName(tpeName, Option(impl.getFormat()), ScalaType(impl))
    }
  }

  def responseMetaType[T <: Response](response: T): Target[Type] = {
    response match {
      case r: RefResponse =>
        propMetaType(r.getSchema())
      case x =>
        Target.error(s"responseMetaType: Unsupported type ${x}")
    }
  }

  case class ParamMeta(tpe: Type, defaultValue: Option[Term])
  def paramMeta[T <: Parameter](param: T): Target[ParamMeta] = {
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
      case x: BodyParameter => for {
        schema <- Target.fromOption(Option(x.getSchema()), "Schema not specified")
        tpe <- modelMetaType(schema)
      } yield ParamMeta(tpe, None)
      case x: HeaderParameter =>
        for {
          tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
        } yield ParamMeta(typeName(tpeName, Option(x.getFormat()), ScalaType(x)), getDefault(x))
      case x: PathParameter =>
        for {
          tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
        } yield ParamMeta(typeName(tpeName, Option(x.getFormat()), ScalaType(x)), getDefault(x))
      case x: QueryParameter =>
        for {
          tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
        } yield ParamMeta(typeName(tpeName, Option(x.getFormat()), ScalaType(x)), getDefault(x))
      case x: CookieParameter =>
        for {
          tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
        } yield ParamMeta(typeName(tpeName, Option(x.getFormat()), ScalaType(x)), getDefault(x))
      case x: FormParameter =>
        for {
          tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
        } yield ParamMeta(typeName(tpeName, Option(x.getFormat()), ScalaType(x)), getDefault(x))
      case r: RefParameter =>
        for {
          tpeName <- Target.fromOption(Option(r.getSimpleRef()), "$ref not defined")
        } yield ParamMeta(Type.Name(tpeName), None)
      case x: SerializableParameter =>
        for {
          tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
        } yield ParamMeta(typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None)
      case x =>
        Target.error(s"Unsure how to handle ${x}")
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
    case ctor@Init(Type.Name(name), _, _) if name != "this" => ctor.copy(tpe=Type.Name(escapeReserved(name))) // Literal "this" in ctor names is OK
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

  def propMetaType[T <: Property](property: T): Target[Type] = {
    propMeta[T](property).map(_.tpe)
  }

  case class PropMeta(tpe: Type, classDep: Option[Term.Name], defaultValue: Option[Term])
  def propMeta[T <: Property](property: T): Target[PropMeta] = {
    property match {
      case p: ArrayProperty =>
        val title = Option(p.getTitle()).getOrElse("Unnamed array")
        for {
          items <- Target.fromOption(Option(p.getItems()), s"${title} has no items")
          rec <- propMeta(items)
          PropMeta(inner, dep, _) = rec
        } yield PropMeta(t"IndexedSeq[${inner}]", dep, None)
      case m: MapProperty =>
        for {
          rec <- propMeta(m.getAdditionalProperties)
          PropMeta(inner, dep, _) = rec
        } yield PropMeta(t"Map[String, ${inner}]", dep, None)
      case o: ObjectProperty =>
        Target.pure(PropMeta(t"Json", None, None)) // TODO: o.getProperties
      case r: RefProperty =>
        Target.pure(PropMeta(Type.Name(r.getSimpleRef), Some(Term.Name(r.getSimpleRef)), None))

      case b: BooleanProperty =>
        Target.pure(PropMeta(t"Boolean", None, Default(b).extract[Boolean].map(Lit.Boolean(_))))
      case s: StringProperty =>
        Target.pure(PropMeta(typeName("string", Option(s.getFormat()), ScalaType(s)), None, Default(s).extract[String].map(Lit.String(_))))

      case d: DateProperty =>
        Target.pure(PropMeta(t"java.time.LocalDate", None, None))
      case d: DateTimeProperty =>
        Target.pure(PropMeta(t"java.time.OffsetDateTime", None, None))

      case l: LongProperty =>
        Target.pure(PropMeta(t"Long", None, Default(l).extract[Long].map(Lit.Long(_))))
      case i: IntegerProperty =>
        Target.pure(PropMeta(t"Int", None, Default(i).extract[Int].map(Lit.Int(_))))
      case f: FloatProperty =>
        Target.pure(PropMeta(t"Float", None, Default(f).extract[Float].map(Lit.Float(_))))
      case d: DoubleProperty =>
        Target.pure(PropMeta(t"Double", None, Default(d).extract[Double].map(Lit.Double(_))))
      case d: DecimalProperty =>
        Target.pure(PropMeta(t"BigDecimal", None, None))
      case p: AbstractProperty if p.getType.toLowerCase == "integer" =>
        Target.pure(PropMeta(t"BigInt", None, None))
      case p: AbstractProperty if p.getType.toLowerCase == "number" =>
        Target.pure(PropMeta(t"BigDecimal", None, None))
      case p: AbstractProperty if p.getType.toLowerCase == "string" =>
        Target.pure(PropMeta(t"String", None, None))
      case x =>
        Target.error(s"Unsupported swagger class ${x.getClass().getName()} (${x})")
    }
  }

  /*
    Required \ Default  || Defined  || Undefined / NULL ||
    =====================================================
    TRUE                || a: T = v || a: T             ||
    FALSE / NULL        || a: T = v || a: Opt[T] = None ||
  */

  private[this] val successCodesWithEntities = List(200, 201, 202, 203, 206, 226).map(_.toString)
  private[this] val successCodesWithoutEntities = List(204, 205).map(_.toString)

  private[this] def getBestSuccessResponse(responses: JMap[String, Response]): Option[Response] =
    successCodesWithEntities.find(responses.containsKey).flatMap(code => Option(responses.get(code)))
  private[this] def hasEmptySuccessType(responses: JMap[String, Response]): Boolean =
    successCodesWithoutEntities.exists(responses.containsKey)

  def getResponseType(httpMethod: HttpMethod, operation: Operation, ignoredType: Type = t"IgnoredEntity"): Target[Type] = {
    if (httpMethod == HttpMethod.GET || httpMethod == HttpMethod.PUT || httpMethod == HttpMethod.POST) {
      Option(operation.getResponses).flatMap { responses =>
        getBestSuccessResponse(responses)
          .flatMap(resp => Option(resp.getSchema))
          .map(SwaggerUtil.propMetaType)
          .orElse(if (hasEmptySuccessType(responses)) Some(Target.pure(ignoredType)) else None)
      }.getOrElse(Target.pure(ignoredType))
    } else {
      Target.pure(ignoredType)
    }
  }

  object paths {
    def generateUrlPathParams(path: String, pathArgs: List[ScalaParameter]): Target[Term] = {
      import atto._, Atto._

      val term: Parser[Term.Apply] = many(notChar('}')).map(_.mkString("")).flatMap({ term =>
        pathArgs
          .find(_.argName.value == term)
          .fold[Parser[Term.Apply]](
            err(s"Unable to find argument ${term}")
          )({ case ScalaParameter(_, _, paramName, _, _) =>
            ok(q"Formatter.addPath(${paramName})")
          })
      })
      val variable: Parser[Term.Apply] = char('{') ~> term <~ char('}')
      val other: Parser[String] = many1(notChar('{')).map(_.toList.mkString)
      val pattern: Parser[List[Either[String, Term.Apply]]] = many(either(variable, other).map(_.swap: Either[String, Term.Apply]))

      for {
        parts <- pattern.parseOnly(path).either.fold(Target.error(_), Target.pure(_))
        result = parts.map({
          case Left(part) => Lit.String(part)
          case Right(term) => term
        }).foldLeft[Term](q"host + basePath")({ case (a, b) => q"${a} + ${b}" })
      } yield result
    }
  }
}
