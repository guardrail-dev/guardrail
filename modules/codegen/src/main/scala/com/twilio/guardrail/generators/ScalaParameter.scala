package com.twilio.guardrail
package generators

import _root_.io.swagger.models.Model
import _root_.io.swagger.models.parameters.Parameter
import com.twilio.guardrail.extract.{ Default, ScalaFileHashAlgorithm, ScalaType }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms.{ ScalaTerm, ScalaTerms, SwaggerTerm, SwaggerTerms }
import com.twilio.guardrail.terms.framework.{ FrameworkTerm, FrameworkTerms }
import java.util.Locale

import scala.meta._
import cats.MonadError
import cats.implicits._
import cats.arrow.FunctionK
import cats.free.Free
import cats.data.{ EitherK, EitherT }

class GeneratorSettings[L <: LA](val fileType: L#Type, val jsonType: L#Type)
case class RawParameterName private[generators] (value: String)
class ScalaParameters[L <: LA](val parameters: List[ScalaParameter[L]]) {
  val filterParamBy     = ScalaParameter.filterParams(parameters)
  val headerParams      = filterParamBy("header")
  val pathParams        = filterParamBy("path")
  val queryStringParams = filterParamBy("query")
  val bodyParams        = filterParamBy("body").headOption
  val formParams        = filterParamBy("formData")
}
class ScalaParameter[L <: LA] private[generators] (
    val in: Option[String],
    val param: L#MethodParameter,
    val paramName: L#TermName,
    val argName: RawParameterName,
    val argType: L#Type,
    val required: Boolean,
    val hashAlgorithm: Option[String],
    val isFile: Boolean
) {
  override def toString: String =
    s"ScalaParameter(${in}, ${param}, ${paramName}, ${argName}, ${argType})"

  def withType(newArgType: L#Type): ScalaParameter[L] =
    new ScalaParameter[L](in, param, paramName, argName, newArgType, required, hashAlgorithm, isFile)
}
object ScalaParameter {
  def unapply[L <: LA](param: ScalaParameter[L]): Option[(Option[String], L#MethodParameter, L#TermName, RawParameterName, L#Type)] =
    Some((param.in, param.param, param.paramName, param.argName, param.argType))

  @deprecated("Use fromParameter instead", "0.41.2")
  def fromParam(param: Term.Param): ScalaParameter[ScalaLanguage] = param match {
    case param @ Term.Param(_, name, decltype, _) =>
      val (tpe, required): (Type, Boolean) = decltype
        .flatMap({
          case tpe @ t"Option[$_]" => Some((tpe, false))
          case Type.ByName(tpe)    => Some((tpe, true))
          case tpe @ Type.Name(_)  => Some((tpe, true))
          case _                   => None
        })
        .getOrElse((t"Nothing", true))
      new ScalaParameter[ScalaLanguage](None, param, Term.Name(name.value), RawParameterName(name.value), tpe, required, None, false)
  }

  def fromParameter(protocolElems: List[StrictProtocolElems[ScalaLanguage]]): Parameter => Target[ScalaParameter[ScalaLanguage]] = {
    type F[T] = EitherK[ScalaTerm[ScalaLanguage, ?], EitherK[FrameworkTerm[ScalaLanguage, ?], SwaggerTerm[ScalaLanguage, ?], ?], T]
    val interp = ScalaGenerator.ScalaInterp.or(AkkaHttpGenerator.FrameworkInterp.or(SwaggerGenerator.SwaggerInterp));
    { parameter =>
      fromParameterF[ScalaLanguage, F](protocolElems).apply(parameter).foldMap(interp)
    }
  }

  def fromParameterF[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Parameter => Free[F, ScalaParameter[L]] = { parameter =>
    import Fw._
    import Sc._
    import Sw._

    def toCamelCase(s: String): String = {
      val fromSnakeOrDashed =
        "[_-]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))
      "^([A-Z])".r
        .replaceAllIn(fromSnakeOrDashed, m => m.group(1).toLowerCase(Locale.US))
    }

    def paramMeta(param: Parameter): Free[F, SwaggerUtil.ResolvedType[L]] = {
      import _root_.io.swagger.models.parameters._

      def getDefaultF[U <: AbstractSerializableParameter[U]: Default.GetDefault](p: U): Free[F, Option[L#Term]] =
        Option(p.getType)
          .flatTraverse[Free[F, ?], L#Term]({ _type =>
            val fmt = Option(p.getFormat)
            (_type, fmt) match {
              case ("string", None) =>
                Default(p).extract[String].traverse(litString(_))
              case ("number", Some("float")) =>
                Default(p).extract[Float].traverse(litFloat(_))
              case ("number", Some("double")) =>
                Default(p).extract[Double].traverse(litDouble(_))
              case ("integer", Some("int32")) =>
                Default(p).extract[Int].traverse(litInt(_))
              case ("integer", Some("int64")) =>
                Default(p).extract[Long].traverse(litLong(_))
              case ("boolean", None) =>
                Default(p).extract[Boolean].traverse(litBoolean(_))
              case x => Free.pure(Option.empty[L#Term])
            }
          })

      param match {
        case x: BodyParameter =>
          getBodyParameterSchema(x).flatMap(x => SwaggerUtil.modelMetaTypeF[L, F](x))

        case x: HeaderParameter =>
          getHeaderParameterType(x).flatMap(
            tpeName => (SwaggerUtil.typeNameF[L, F](tpeName, Option(x.getFormat()), ScalaType(x)), getDefaultF(x)).mapN(SwaggerUtil.Resolved[L](_, None, _))
          )

        case x: PathParameter =>
          getPathParameterType(x)
            .flatMap(
              tpeName => (SwaggerUtil.typeNameF[L, F](tpeName, Option(x.getFormat()), ScalaType(x)), getDefaultF(x)).mapN(SwaggerUtil.Resolved[L](_, None, _))
            )

        case x: QueryParameter =>
          getQueryParameterType(x)
            .flatMap(
              tpeName => (SwaggerUtil.typeNameF[L, F](tpeName, Option(x.getFormat()), ScalaType(x)), getDefaultF(x)).mapN(SwaggerUtil.Resolved[L](_, None, _))
            )

        case x: CookieParameter =>
          getCookieParameterType(x)
            .flatMap(
              tpeName => (SwaggerUtil.typeNameF[L, F](tpeName, Option(x.getFormat()), ScalaType(x)), getDefaultF(x)).mapN(SwaggerUtil.Resolved[L](_, None, _))
            )

        case x: FormParameter =>
          getFormParameterType(x)
            .flatMap(
              tpeName => (SwaggerUtil.typeNameF[L, F](tpeName, Option(x.getFormat()), ScalaType(x)), getDefaultF(x)).mapN(SwaggerUtil.Resolved[L](_, None, _))
            )

        case r: RefParameter =>
          getRefParameterRef(r)
            .map(SwaggerUtil.Deferred(_): SwaggerUtil.ResolvedType[L])

        case x: SerializableParameter =>
          getSerializableParameterType(x)
            .flatMap(tpeName => SwaggerUtil.typeNameF[L, F](tpeName, Option(x.getFormat()), ScalaType(x)).map(SwaggerUtil.Resolved[L](_, None, None)))

        case x =>
          fallbackParameterHandler(x)
      }
    }

    for {
      meta     <- paramMeta(parameter)
      resolved <- SwaggerUtil.ResolvedType.resolveF[L, F](meta, protocolElems)
      SwaggerUtil.Resolved(paramType, _, baseDefaultValue) = resolved

      required = parameter.getRequired()
      declType <- if (!required) {
        liftOptionalType(paramType)
      } else {
        Free.pure[F, L#Type](paramType)
      }

      enumDefaultValue <- extractTypeName(paramType).flatMap(_.fold(baseDefaultValue.traverse(Free.pure[F, L#Term] _)) { tpe =>
        protocolElems
          .flatTraverse({
            case x @ EnumDefinition(_, _tpeName, _, _, _) =>
              for {
                areEqual <- typeNamesEqual(tpe, _tpeName)
              } yield if (areEqual) List(x) else List.empty[EnumDefinition[L]]
            case _ => Free.pure[F, List[EnumDefinition[L]]](List.empty)
          })
          .flatMap(_.headOption.fold[Free[F, Option[L#Term]]](baseDefaultValue.traverse(Free.pure _)) { x =>
            baseDefaultValue.traverse(lookupEnumDefaultValue(tpe, _, x.elems).flatMap(widenTermSelect))
          })
        })

      defaultValue <- if (!required) {
        (enumDefaultValue.traverse(liftOptionalTerm), emptyOptionalTerm().map(Option.apply _)).mapN(_.orElse(_))
      } else {
        Free.pure[F, Option[L#Term]](enumDefaultValue)
      }

      name <- getParameterName(parameter)

      paramName <- pureTermName(toCamelCase(name))
      param     <- pureMethodParameter(paramName, declType, defaultValue)

      ftpe       <- fileType(None)
      isFileType <- typesEqual(paramType, ftpe)
    } yield {
      new ScalaParameter[L](Option(parameter.getIn),
                            param,
                            paramName,
                            RawParameterName(name),
                            declType,
                            required,
                            ScalaFileHashAlgorithm(parameter),
                            isFileType)
    }
  }

  def fromParameters(protocolElems: List[StrictProtocolElems[ScalaLanguage]]): List[Parameter] => Target[List[ScalaParameter[ScalaLanguage]]] = { params =>
    for {
      parameters <- params.traverse(fromParameter(protocolElems))
      counts = parameters.groupBy(_.paramName.value).mapValues(_.length)
    } yield
      parameters.map { param =>
        val Term.Name(name) = param.paramName
        if (counts.getOrElse(name, 0) > 1) {
          val escapedName =
            Term.Name(param.argName.value)
          new ScalaParameter[ScalaLanguage](
            param.in,
            param.param.copy(name = escapedName),
            escapedName,
            param.argName,
            param.argType,
            param.required,
            param.hashAlgorithm,
            param.isFile
          )
        } else param
      }
  }

  /**
    * Create method parameters from Swagger's Path parameters list. Use Option for non-required parameters.
    * @param params
    * @return
    */
  def filterParams[L <: LA](params: List[ScalaParameter[L]]): String => List[ScalaParameter[L]] = { in =>
    params.filter(_.in == Some(in))
  }
}
