package com.twilio.guardrail
package generators

import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.oas.models.parameters._
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.extract.{ Default, FileHashAlgorithm }
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import cats.implicits._
import cats.free.Free
import com.twilio.guardrail.SwaggerUtil.ResolvedType

case class RawParameterName private[generators] (value: String)
case class RawParameterType private[generators] (tpe: Option[String], format: Option[String])
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
    val rawType: RawParameterType,
    val required: Boolean,
    val hashAlgorithm: Option[String],
    val isFile: Boolean
) {
  override def toString: String =
    s"ScalaParameter($in, $param, $paramName, $argName, $argType)"

  def withType(newArgType: L#Type, rawType: Option[String] = this.rawType.tpe, rawFormat: Option[String] = this.rawType.format): ScalaParameter[L] =
    new ScalaParameter[L](in, param, paramName, argName, newArgType, RawParameterType(rawType, rawFormat), required, hashAlgorithm, isFile)
}
object ScalaParameter {
  def unapply[L <: LA](param: ScalaParameter[L]): Option[(Option[String], L#MethodParameter, L#TermName, RawParameterName, L#Type)] =
    Some((param.in, param.param, param.paramName, param.argName, param.argType))

  def fromParameter[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Parameter => Free[F, ScalaParameter[L]] = { parameter =>
    import Fw._
    import Sc._
    import Sw._

    def paramMeta(param: Parameter): Free[F, SwaggerUtil.ResolvedType[L]] = {
      def getDefault[U <: Parameter: Default.GetDefault](_type: String, fmt: Option[String], p: U): Free[F, Option[L#Term]] =
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

      def resolveParam(param: Parameter, typeFetcher: Parameter => Free[F, String]): Free[F, ResolvedType[L]] =
        for {
          tpeName <- typeFetcher(param)
          schema = Option[Schema[_]](param.getSchema)
          fmt    = schema.flatMap(s => Option(s.getFormat))
          customParamTypeName  <- SwaggerUtil.customTypeName(param)
          customSchemaTypeName <- schema.flatTraverse(SwaggerUtil.customTypeName(_: Schema[_]))
          customTypeName = customSchemaTypeName.orElse(customParamTypeName)
          res <- (SwaggerUtil.typeName[L, F](Tracker.hackyAdapt(Option(tpeName), Vector.empty),
                                             Tracker.hackyAdapt(Option(param.format()), Vector.empty),
                                             customTypeName),
                  getDefault(tpeName, fmt, param))
            .mapN(SwaggerUtil.Resolved[L](_, None, _, Some(tpeName), fmt))
        } yield res

      def paramHasRefSchema(p: Parameter): Boolean = Option(p.getSchema).exists(s => Option(s.get$ref()).nonEmpty)

      param match {
        case r: Parameter if r.isRef =>
          getRefParameterRef(Tracker.hackyAdapt(r, Vector.empty))
            .map(SwaggerUtil.Deferred(_): SwaggerUtil.ResolvedType[L])

        case r: Parameter if paramHasRefSchema(r) =>
          getSimpleRef(r.getSchema)
            .map(SwaggerUtil.Deferred(_): SwaggerUtil.ResolvedType[L])

        case x: Parameter if x.isInBody =>
          getBodyParameterSchema(x)
            .flatMap(x => SwaggerUtil.modelMetaType[L, F](x))

        case x: Parameter if x.isInHeader =>
          resolveParam(x, getHeaderParameterType)

        case x: Parameter if x.isInPath =>
          resolveParam(x, getPathParameterType)

        case x: Parameter if x.isInQuery =>
          resolveParam(x, getQueryParameterType)

        case x: Parameter if x.isInCookies =>
          resolveParam(x, getCookieParameterType)

        case x: Parameter if x.isInFormData =>
          resolveParam(x, getFormParameterType)

        case x =>
          fallbackParameterHandler(x)
      }
    }

    log.function(s"fromParameter")(for {
      _                                                                        <- log.debug(parameter.showNotNull)
      meta                                                                     <- paramMeta(parameter)
      SwaggerUtil.Resolved(paramType, _, baseDefaultValue, rawType, rawFormat) <- SwaggerUtil.ResolvedType.resolve[L, F](meta, protocolElems)

      required = Option[java.lang.Boolean](parameter.getRequired()).fold(false)(identity)
      declType <- if (!required) {
        liftOptionalType(paramType)
      } else {
        Free.pure[F, L#Type](paramType)
      }

      enumDefaultValue <- extractTypeName(paramType).flatMap(_.fold(baseDefaultValue.traverse(Free.pure[F, L#Term] _)) { tpe =>
        protocolElems
          .flatTraverse({
            case x @ EnumDefinition(_, _tpeName, _, _, _, _) =>
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

      paramName <- pureTermName(name.toCamelCase)
      param     <- pureMethodParameter(paramName, declType, defaultValue)

      ftpe       <- fileType(None)
      isFileType <- typesEqual(paramType, ftpe)
    } yield {
      new ScalaParameter[L](
        Option(parameter.getIn),
        param,
        paramName,
        RawParameterName(name),
        declType,
        RawParameterType(rawType, rawFormat),
        required,
        FileHashAlgorithm(parameter),
        isFileType
      )
    })
  }

  def fromParameters[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): List[Parameter] => Free[F, List[ScalaParameter[L]]] = { params =>
    import Sc._
    for {
      parameters <- params.traverse(fromParameter(protocolElems))
      counts     <- parameters.traverse(param => extractTermName(param.paramName)).map(_.groupBy(identity).mapValues(_.length))
      result <- parameters.traverse { param =>
        extractTermName(param.paramName).flatMap { name =>
          if (counts.getOrElse(name, 0) > 1) {
            pureTermName(param.argName.value).flatMap { escapedName =>
              alterMethodParameterName(param.param, escapedName).map { newParam =>
                new ScalaParameter[L](
                  param.in,
                  newParam,
                  escapedName,
                  param.argName,
                  param.argType,
                  param.rawType,
                  param.required,
                  param.hashAlgorithm,
                  param.isFile
                )
              }
            }
          } else Free.pure[F, ScalaParameter[L]](param)
        }
      }
    } yield result
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
