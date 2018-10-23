package com.twilio.guardrail

import _root_.io.swagger.models._
import _root_.io.swagger.models.parameters._
import _root_.io.swagger.models.properties._
import cats.syntax.either._
import cats.{ FlatMap, Foldable }
import cats.instances.list._
import com.twilio.guardrail.extract.{ Default, ScalaType }
import com.twilio.guardrail.generators.{ GeneratorSettings, ScalaParameter }
import java.util.{ Map => JMap }
import scala.language.reflectiveCalls
import scala.meta._

object SwaggerUtil {
  sealed trait ResolvedType
  case class Resolved(tpe: Type, classDep: Option[Term.Name], defaultValue: Option[Term]) extends ResolvedType
  sealed trait LazyResolvedType                                                           extends ResolvedType
  case class Deferred(value: String)                                                      extends LazyResolvedType
  case class DeferredArray(value: String)                                                 extends LazyResolvedType
  case class DeferredMap(value: String)                                                   extends LazyResolvedType
  object ResolvedType {
    implicit class FoldableExtension[F[_]](F: Foldable[F]) {
      import cats.{ Alternative, Monoid }
      def partitionEither[A, B, C](value: F[A])(f: A => Either[B, C])(implicit A: Alternative[F]): (F[B], F[C]) = {
        import cats.instances.tuple._

        implicit val mb: Monoid[F[B]] = A.algebra[B]
        implicit val mc: Monoid[F[C]] = A.algebra[C]

        F.foldMap(value)(
          a =>
            f(a) match {
              case Left(b)  => (A.pure(b), A.empty[C])
              case Right(c) => (A.empty[B], A.pure(c))
          }
        )
      }
    }

    def resolve_(values: List[(String, ResolvedType)]): Target[List[(String, Resolved)]] = {
      val (lazyTypes, resolvedTypes) = Foldable[List].partitionEither(values) {
        case (clsName, x: Resolved)         => Right((clsName, x))
        case (clsName, x: LazyResolvedType) => Left((clsName, x))
      }

      def lookupTypeName(clsName: String, tpeName: String, resolvedTypes: List[(String, Resolved)])(f: Type => Type): Option[(String, Resolved)] =
        resolvedTypes
          .find(_._1 == tpeName)
          .map(_._2.tpe)
          .map(x => (clsName, Resolved(f(x), None, None)))

      FlatMap[Target]
        .tailRecM[(List[(String, LazyResolvedType)], List[(String, Resolved)]), List[(String, Resolved)]]((lazyTypes, resolvedTypes)) {
          case (lazyTypes, resolvedTypes) =>
            if (lazyTypes.isEmpty) {
              Target.pure(Right(resolvedTypes))
            } else {
              val (newLazyTypes, newResolvedTypes) =
                Foldable[List].partitionEither(lazyTypes) {
                  case x @ (clsName, Deferred(tpeName)) =>
                    Either.fromOption(lookupTypeName(clsName, tpeName, resolvedTypes)(identity), x)
                  case x @ (clsName, DeferredArray(tpeName)) =>
                    Either.fromOption(lookupTypeName(clsName, tpeName, resolvedTypes)(tpe => t"IndexedSeq[${tpe}]"), x)
                  case x @ (clsName, DeferredMap(tpeName)) =>
                    Either.fromOption(lookupTypeName(clsName, tpeName, resolvedTypes)(tpe => t"Map[String, ${tpe}]"), x)
                }

              Target.pure(Left((newLazyTypes, resolvedTypes ++ newResolvedTypes)))
            }
        }
    }

    def resolve(value: ResolvedType, protocolElems: List[StrictProtocolElems]): Target[Resolved] =
      value match {
        case x @ Resolved(tpe, _, default) => Target.pure(x)
        case Deferred(name) =>
          Target
            .fromOption(protocolElems.find(_.name == name), s"Unable to resolve ${name}")
            .map {
              case RandomType(name, tpe) => Resolved(tpe, None, None)
              case ClassDefinition(name, tpe, cls, companion, _) =>
                Resolved(tpe, None, None)
              case EnumDefinition(name, tpe, elems, cls, companion) =>
                Resolved(tpe, None, None)
              case ADT(_, tpe, _, _) =>
                Resolved(tpe, None, None)
            }
        case DeferredArray(name) =>
          Target
            .fromOption(protocolElems.find(_.name == name), s"Unable to resolve ${name}")
            .map {
              case RandomType(name, tpe) =>
                Resolved(t"IndexedSeq[${tpe}]", None, None)
              case ClassDefinition(name, tpe, cls, companion, _) =>
                Resolved(t"IndexedSeq[${tpe}]", None, None)
              case EnumDefinition(name, tpe, elems, cls, companion) =>
                Resolved(t"IndexedSeq[${tpe}]", None, None)
              case ADT(_, tpe, _, _) =>
                Resolved(t"IndexedSeq[$tpe]", None, None)
            }
        case DeferredMap(name) =>
          Target
            .fromOption(protocolElems.find(_.name == name), s"Unable to resolve ${name}")
            .map {
              case RandomType(name, tpe) =>
                Resolved(t"Map[String, ${tpe}]", None, None)
              case ClassDefinition(_, tpe, _, _, _) =>
                Resolved(t"Map[String, ${tpe}]", None, None)
              case EnumDefinition(_, tpe, _, _, _) =>
                Resolved(t"Map[String, ${tpe}]", None, None)
              case ADT(_, tpe, _, _) =>
                Resolved(t"Map[String, $tpe]", None, None)
            }
      }
  }

  def modelMetaType[T <: Model](model: T): Target[ResolvedType] =
    Target.getGeneratorSettings.flatMap { implicit gs =>
      model match {
        case ref: RefModel =>
          for {
            ref <- Target.fromOption(Option(ref.getSimpleRef()), "Unspecified $ref")
          } yield Deferred(ref)
        case arr: ArrayModel =>
          for {
            items <- Target.fromOption(Option(arr.getItems()), "items.type unspecified")
            meta  <- propMeta(items)
            res <- meta match {
              case Resolved(inner, dep, default) =>
                Target.pure(Resolved(t"IndexedSeq[${inner}]", dep, default.map(x => q"IndexedSeq(${x})")))
              case Deferred(tpe) => Target.pure(DeferredArray(tpe))
              case DeferredArray(_) =>
                Target.error("FIXME: Got an Array of Arrays, currently not supported")
              case DeferredMap(_) =>
                Target.error("FIXME: Got an Array of Maps, currently not supported")
            }
          } yield res
        case impl: ModelImpl =>
          for {
            tpeName <- Target.fromOption(
              Option(impl.getType()),
              s"Unable to resolve type for ${impl.getDescription()} (${impl
                .getEnum()} ${impl.getName()} ${impl.getType()} ${impl.getFormat()})"
            )
          } yield Resolved(typeName(tpeName, Option(impl.getFormat()), ScalaType(impl)), None, None)
      }
    }

  case class ParamMeta(tpe: Type, defaultValue: Option[Term])
  def paramMeta[T <: Parameter](param: T): Target[ParamMeta] = {
    def getDefault[U <: AbstractSerializableParameter[U]: Default.GetDefault](p: U): Option[Term] =
      (
        Option(p.getType)
          .flatMap { _type =>
            val fmt = Option(p.getFormat)
            (_type, fmt) match {
              case ("string", None) =>
                Default(p).extract[String].map(Lit.String(_))
              case ("number", Some("float")) =>
                Default(p).extract[Float].map(Lit.Float(_))
              case ("number", Some("double")) =>
                Default(p).extract[Double].map(Lit.Double(_))
              case ("integer", Some("int32")) =>
                Default(p).extract[Int].map(Lit.Int(_))
              case ("integer", Some("int64")) =>
                Default(p).extract[Long].map(Lit.Long(_))
              case ("boolean", None) =>
                Default(p).extract[Boolean].map(Lit.Boolean(_))
              case x => None
            }
          }
      )

    Target.getGeneratorSettings.flatMap { implicit gs =>
      param match {
        case x: BodyParameter =>
          for {
            schema <- Target.fromOption(Option(x.getSchema()), "Schema not specified")
            tpe    <- modelMetaType(schema)
            meta <- tpe match {
              case SwaggerUtil.Resolved(tpe, _, _) =>
                Target.pure(ParamMeta(tpe, None))
              case xs => Target.error(s"Unresolved references: ${xs}")
            }
          } yield meta
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
  }

  // Standard type conversions, as documented in http://swagger.io/specification/#data-types-12
  def typeName(typeName: String, format: Option[String], customType: Option[String])(implicit gs: GeneratorSettings): Type = {
    def log(fmt: Option[String], t: Type): Type = {
      fmt.foreach { fmt =>
        println(s"Warning: Deprecated behavior: Unsupported type '$fmt', falling back to $t. Please switch definitions to x-scala-type for custom types")
      }

      t
    }
    def liftCustomType(s: String): Option[Type] = {
      val tpe = s.trim
      if (tpe.nonEmpty) {
        tpe
          .parse[Type]
          .fold({ err =>
            println(s"Warning: Unparsable x-scala-type: ${tpe} ${err}")
            None
          }, Option.apply _)
      } else None
    }

    customType.flatMap(liftCustomType _).getOrElse {
      (typeName, format) match {
        case ("string", Some("date"))      => t"java.time.LocalDate"
        case ("string", Some("date-time")) => t"java.time.OffsetDateTime"
        case ("string", o @ Some(fmt))     => log(o, Type.Name(fmt))
        case ("string", None)              => log(None, t"String")
        case ("number", Some("float"))     => t"Float"
        case ("number", Some("double"))    => t"Double"
        case ("number", fmt)               => log(fmt, t"BigDecimal")
        case ("integer", Some("int32"))    => t"Int"
        case ("integer", Some("int64"))    => t"Long"
        case ("integer", fmt)              => log(fmt, t"BigInt")
        case ("boolean", fmt)              => log(fmt, t"Boolean")
        case ("array", fmt)                => log(fmt, t"Iterable[String]")
        case ("file", o @ Some(fmt))       => log(o, Type.Name(fmt))
        case ("file", fmt)                 => log(fmt, gs.fileType)
        case ("object", fmt)               => log(fmt, gs.jsonType)
        case (x, fmt) => {
          println(s"Fallback: ${x} (${fmt})")
          Type.Name(x)
        }
      }
    }
  }

  def propMeta[T <: Property](property: T): Target[ResolvedType] =
    Target.getGeneratorSettings.flatMap { implicit gs =>
      property match {
        case p: ArrayProperty =>
          val title = Option(p.getTitle()).getOrElse("Unnamed array")
          for {
            items <- Target.fromOption(Option(p.getItems()), s"${title} has no items")
            rec   <- propMeta(items)
            res <- rec match {
              case DeferredMap(_) =>
                Target.error("FIXME: Got an Array of Maps, currently not supported")
              case DeferredArray(_) =>
                Target.error("FIXME: Got an Array of Arrays, currently not supported")
              case Deferred(inner) => Target.pure(DeferredArray(inner))
              case Resolved(inner, dep, default) =>
                Target.pure(Resolved(t"IndexedSeq[${inner}]", dep, default.map(x => q"IndexedSeq(${x})")))
            }
          } yield res
        case m: MapProperty =>
          for {
            rec <- propMeta(m.getAdditionalProperties)
            res <- rec match {
              case DeferredMap(_) =>
                Target.error("FIXME: Got a map of maps, currently not supported")
              case DeferredArray(_) =>
                Target.error("FIXME: Got a map of arrays, currently not supported")
              case Deferred(inner) => Target.pure(DeferredMap(inner))
              case Resolved(inner, dep, _) =>
                Target.pure(Resolved(t"Map[String, ${inner}]", dep, None))
            }
          } yield res
        case o: ObjectProperty =>
          Target.pure(Resolved(gs.jsonType, None, None)) // TODO: o.getProperties
        case r: RefProperty =>
          Target
            .fromOption(Option(r.getSimpleRef()), "Malformed $ref")
            .map(Deferred.apply _)
        case b: BooleanProperty =>
          Target.pure(Resolved(typeName("boolean", None, ScalaType(b)), None, Default(b).extract[Boolean].map(Lit.Boolean(_))))
        case s: StringProperty =>
          Target.pure(Resolved(typeName("string", Option(s.getFormat()), ScalaType(s)), None, Default(s).extract[String].map(Lit.String(_))))

        case d: DateProperty =>
          Target.pure(Resolved(typeName("string", Some("date"), ScalaType(d)), None, None))
        case d: DateTimeProperty =>
          Target.pure(Resolved(typeName("string", Some("date-time"), ScalaType(d)), None, None))

        case l: LongProperty =>
          Target.pure(Resolved(typeName("integer", Some("int64"), ScalaType(l)), None, Default(l).extract[Long].map(Lit.Long(_))))
        case i: IntegerProperty =>
          Target.pure(Resolved(typeName("integer", Some("int32"), ScalaType(i)), None, Default(i).extract[Int].map(Lit.Int(_))))
        case f: FloatProperty =>
          Target.pure(Resolved(typeName("number", Some("float"), ScalaType(f)), None, Default(f).extract[Float].map(Lit.Float(_))))
        case d: DoubleProperty =>
          Target.pure(Resolved(typeName("number", Some("double"), ScalaType(d)), None, Default(d).extract[Double].map(Lit.Double(_))))
        case d: DecimalProperty =>
          Target.pure(Resolved(typeName("number", None, ScalaType(d)), None, None))
        case u: UntypedProperty =>
          Target.pure(Resolved(gs.jsonType, None, None))
        case p: AbstractProperty if Option(p.getType).exists(_.toLowerCase == "integer") =>
          Target.pure(Resolved(typeName("integer", None, ScalaType(p)), None, None))
        case p: AbstractProperty if Option(p.getType).exists(_.toLowerCase == "number") =>
          Target.pure(Resolved(typeName("number", None, ScalaType(p)), None, None))
        case p: AbstractProperty if Option(p.getType).exists(_.toLowerCase == "string") =>
          Target.pure(Resolved(typeName("string", None, ScalaType(p)), None, None))
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

  private[this] val successCodesWithEntities =
    List(200, 201, 202, 203, 206, 226).map(_.toString)
  private[this] val successCodesWithoutEntities = List(204, 205).map(_.toString)

  private[this] def getBestSuccessResponse(responses: JMap[String, Response]): Option[Response] =
    successCodesWithEntities
      .find(responses.containsKey)
      .flatMap(code => Option(responses.get(code)))
  private[this] def hasEmptySuccessType(responses: JMap[String, Response]): Boolean =
    successCodesWithoutEntities.exists(responses.containsKey)

  def getResponseType(httpMethod: HttpMethod, operation: Operation, ignoredType: Type = t"IgnoredEntity"): Target[ResolvedType] =
    if (httpMethod == HttpMethod.GET || httpMethod == HttpMethod.PUT || httpMethod == HttpMethod.POST) {
      Option(operation.getResponses)
        .flatMap { responses =>
          getBestSuccessResponse(responses)
            .flatMap(resp => Option(resp.getSchema))
            .map(propMeta)
            .orElse(
              if (hasEmptySuccessType(responses))
                Some(Target.pure(Resolved(ignoredType, None, None): ResolvedType))
              else None
            )
        }
        .getOrElse(Target.pure(Resolved(ignoredType, None, None)))
    } else {
      Target.pure(Resolved(ignoredType, None, None))
    }

  object paths {
    import atto._, Atto._

    private[this] def lookupName[T](bindingName: String, pathArgs: List[ScalaParameter])(f: ScalaParameter => Parser[T]): Parser[T] =
      pathArgs
        .find(_.argName.value == bindingName)
        .fold[Parser[T]](
          err(s"Unable to find argument ${bindingName}")
        )(param => f(param))

    private[this] val variable: Parser[String] = char('{') ~> many(notChar('}'))
      .map(_.mkString("")) <~ char('}')

    def generateUrlPathParams(path: String, pathArgs: List[ScalaParameter]): Target[Term] = {
      val term: Parser[Term.Apply] = variable.flatMap { binding =>
        lookupName(binding, pathArgs) { param =>
          ok(q"Formatter.addPath(${param.paramName})")
        }
      }
      val other: Parser[String]                             = many1(notChar('{')).map(_.toList.mkString)
      val pattern: Parser[List[Either[String, Term.Apply]]] = many(either(term, other).map(_.swap: Either[String, Term.Apply]))

      for {
        parts <- pattern
          .parseOnly(path)
          .either
          .fold(Target.error(_), Target.pure(_))
        result = parts
          .map({
            case Left(part)  => Lit.String(part)
            case Right(term) => term
          })
          .foldLeft[Term](q"host + basePath")({ case (a, b) => q"${a} + ${b}" })
      } yield result
    }

    class Extractors[T, TN <: T](
        pathSegmentConverter: (ScalaParameter, Option[T]) => Either[String, T],
        buildParamConstraint: ((String, String)) => T,
        joinParams: (T, T) => T,
        stringPath: String => T,
        liftBinding: Term.Name => TN,
        litRegex: (String, Term.Name, String) => T
    ) {
      // (Option[TN], T) is (Option[Binding], Segment)
      type P  = Parser[(Option[TN], T)]
      type LP = Parser[List[(Option[TN], T)]]

      val plainString      = many(noneOf("{}/?")).map(_.mkString)
      val plainNEString    = many1(noneOf("{}/?")).map(_.toList.mkString)
      val stringSegment: P = plainNEString.map(s => (None, stringPath(s)))
      def regexSegment(implicit pathArgs: List[ScalaParameter]): P =
        (plainString ~ variable ~ plainString).flatMap {
          case ((before, binding), after) =>
            lookupName(binding, pathArgs) {
              case param @ ScalaParameter(_, _, paramName, argName, _) =>
                val value = if (before.nonEmpty || after.nonEmpty) {
                  pathSegmentConverter(param, Some(litRegex(before.mkString, paramName, after.mkString)))
                    .fold(err, ok)
                } else {
                  pathSegmentConverter(param, None).fold(err, ok)
                }
                value.map((Some(liftBinding(paramName)), _))
            }
        }

      def segments(implicit pathArgs: List[ScalaParameter]): LP =
        sepBy1(choice(regexSegment(pathArgs), stringSegment), char('/'))
          .map(_.toList)

      val qsValueOnly: Parser[(String, String)] = ok("") ~ (char('=') ~> opt(many(noneOf("&")))
        .map(_.fold("")(_.mkString)))
      val staticQSArg: Parser[(String, String)] = many1(noneOf("=&"))
        .map(_.toList.mkString) ~ opt(char('=') ~> many(noneOf("&")))
        .map(_.fold("")(_.mkString))
      val staticQSTerm: Parser[T] =
        choice(staticQSArg, qsValueOnly).map(buildParamConstraint)
      val trailingSlash: Parser[Boolean] = opt(char('/')).map(_.nonEmpty)
      val staticQS: Parser[Option[T]] = (opt(
        char('?') ~> sepBy1(staticQSTerm, char('&'))
          .map(_.reduceLeft(joinParams))
      ) | opt(char('?')).map { _ =>
        None
      })
      val emptyPath: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))]   = endOfInput ~> ok((List.empty[(Option[TN], T)], (false, None)))
      val emptyPathQS: Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] = ok(List.empty[(Option[TN], T)]) ~ (ok(false) ~ staticQS)
      def pattern(implicit pathArgs: List[ScalaParameter]): Parser[(List[(Option[TN], T)], (Boolean, Option[T]))] =
        (segments ~ (trailingSlash ~ staticQS) <~ endOfInput) | emptyPathQS | emptyPath
    }

    object akkaExtractor
        extends Extractors[Term, Term.Name](
          pathSegmentConverter = {
            case (ScalaParameter(_, param, _, argName, argType), base) =>
              base.fold {
                argType match {
                  case t"String" => Right(q"Segment")
                  case t"Double" => Right(q"DoubleNumber")
                  case t"BigDecimal" =>
                    Right(q"Segment.map(BigDecimal.apply _)")
                  case t"Int"    => Right(q"IntNumber")
                  case t"Long"   => Right(q"LongNumber")
                  case t"BigInt" => Right(q"Segment.map(BigInt.apply _)")
                  case tpe @ Type.Name(_) =>
                    Right(q"Segment.flatMap(str => io.circe.Json.fromString(str).as[${tpe}].toOption)")
                }
              } { segment =>
                argType match {
                  case t"String" => Right(segment)
                  case t"BigDecimal" =>
                    Right(q"${segment}.map(BigDecimal.apply _)")
                  case t"BigInt" => Right(q"${segment}.map(BigInt.apply _)")
                  case tpe @ Type.Name(_) =>
                    Right(q"${segment}.flatMap(str => io.circe.Json.fromString(str).as[${tpe}].toOption)")
                }
              }
          },
          buildParamConstraint = {
            case (k, v) =>
              q" parameter(${Lit.String(k)}).require(_ == ${Lit.String(v)}) "
          },
          joinParams = { (l, r) =>
            q"${l} & ${r}"
          },
          stringPath = Lit.String(_),
          liftBinding = identity,
          litRegex = (before, _, after) =>
            q"""new scala.util.matching.Regex("^" + ${Lit
              .String(before)} + "(.*)" + ${Lit.String(after)} + ${Lit.String("$")})"""
        )

    object http4sExtractor
        extends Extractors[Pat, Term.Name](
          pathSegmentConverter = {
            case (ScalaParameter(_, param, paramName, argName, argType), base) =>
              base.fold[Either[String, Pat]] {
                argType match {
                  case t"String"     => Right(Pat.Var(paramName))
                  case t"Double"     => Right(p"DoubleVar($paramName)")
                  case t"BigDecimal" => Right(p"BigDecimalVar(${Pat.Var(paramName)})")
                  case t"Int"        => Right(p"IntVar(${Pat.Var(paramName)})")
                  case t"Long"       => Right(p"LongVar(${Pat.Var(paramName)})")
                  case t"BigInt"     => Right(p"BigIntVar(${Pat.Var(paramName)})")
                  case tpe @ Type.Name(_) =>
                    Right(p"${Term.Name(s"${tpe}Var")}(${Pat.Var(paramName)})")
                }
              } { _ =>
                //todo add support for regex segment
                Left("Unsupported feature")
              }
          },
          buildParamConstraint = {
            case (k, v) =>
              p"${Term.Name(s"${k.capitalize}Matcher")}(${Lit.String(v)})"
          },
          joinParams = { (l, r) =>
            p"${l} +& ${r}"
          },
          stringPath = Lit.String(_),
          liftBinding = identity,
          litRegex = (before, _, after) =>
            //todo add support for regex segment
            throw new UnsupportedOperationException
        )

    def generateUrlAkkaPathExtractors(path: String, pathArgs: List[ScalaParameter]): Target[Term] = {
      import akkaExtractor._
      for {
        partsQS <- pattern(pathArgs)
          .parse(path)
          .done
          .either
          .fold(Target.error(_), Target.pure(_))
        (parts, (trailingSlash, queryParams)) = partsQS
        (directive, bindings) = parts
          .foldLeft[(Term, List[Term.Name])]((q"pathEnd", List.empty))({
            case ((q"pathEnd   ", bindings), (termName, b)) =>
              (q"path(${b}       )", bindings ++ termName)
            case ((q"path(${a })", bindings), (termName, c)) =>
              (q"path(${a} / ${c})", bindings ++ termName)
          })
        trailingSlashed = if (trailingSlash) {
          directive match {
            case q"path(${a })" => q"pathPrefix(${a}) & pathEndOrSingleSlash"
            case q"pathEnd"     => q"pathEndOrSingleSlash"
          }
        } else directive
        result = queryParams.fold(trailingSlashed) { qs =>
          q"${trailingSlashed} & ${qs}"
        }
      } yield result
    }

    def generateUrlHttp4sPathExtractors(path: String, pathArgs: List[ScalaParameter]): Target[(Pat, Option[Pat])] = {
      import http4sExtractor._
      for {
        partsQS <- pattern(pathArgs)
          .parse(path)
          .done
          .either
          .fold(Target.error(_), Target.pure(_))
        (parts, (trailingSlash, queryParams)) = partsQS
        (directive, bindings) = parts
          .foldLeft[(Pat, List[Term.Name])]((p"${Term.Name("Root")}", List.empty))({
            case ((acc, bindings), (termName, c)) =>
              (p"$acc / ${c}", bindings ++ termName)
          })
        trailingSlashed = if (trailingSlash) {
          p"$directive / ${Lit.String("")}"
        } else directive
      } yield (trailingSlashed, queryParams)
    }
  }
}
