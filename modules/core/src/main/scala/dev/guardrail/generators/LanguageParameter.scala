package dev.guardrail.generators

import cats.Monad
import cats.syntax.all._
import io.swagger.v3.oas.models.media
import io.swagger.v3.oas.models.parameters._
import io.swagger.v3.oas.models.Components

import dev.guardrail._
import dev.guardrail.core.{ ReifiedRawType, ResolvedType, Tracker }
import dev.guardrail.core.extract.{ CustomTypeName, Default, FileHashAlgorithm }
import dev.guardrail.core.resolvers.ModelResolver
import dev.guardrail.generators.syntax._
import dev.guardrail.languages.LA
import dev.guardrail.shims._
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SchemaLiteral, SchemaRef, SwaggerTerms }

case class RawParameterName private[generators] (value: String)
class LanguageParameters[L <: LA](val parameters: List[LanguageParameter[L]]) {
  val filterParamBy: String => List[LanguageParameter[L]] = LanguageParameter.filterParams(parameters)
  val headerParams: List[LanguageParameter[L]]            = filterParamBy("header")
  val pathParams: List[LanguageParameter[L]]              = filterParamBy("path")
  val queryStringParams: List[LanguageParameter[L]]       = filterParamBy("query")
  val bodyParams: Option[LanguageParameter[L]]            = filterParamBy("body").headOption
  val formParams: List[LanguageParameter[L]]              = filterParamBy("formData")
}
class LanguageParameter[L <: LA] private[generators] (
    val in: Option[String],
    val param: L#MethodParameter,
    val paramName: L#TermName,
    val argName: RawParameterName,
    val argType: L#Type,
    val rawType: ReifiedRawType,
    val required: Boolean,
    val hashAlgorithm: Option[String],
    val isFile: Boolean
) {
  override def toString: String =
    s"LanguageParameter($in, $param, $paramName, $argName, $argType)"

  def withType(newArgType: L#Type): LanguageParameter[L] =
    new LanguageParameter[L](in, param, paramName, argName, newArgType, rawType, required, hashAlgorithm, isFile)

  def withParamName(newParamName: L#TermName): LanguageParameter[L] =
    new LanguageParameter[L](in, param, newParamName, argName, argType, rawType, required, hashAlgorithm, isFile)
}
object LanguageParameter {
  def unapply[L <: LA](param: LanguageParameter[L]): Some[(Option[String], L#MethodParameter, L#TermName, RawParameterName, L#Type)] =
    Some((param.in, param.param, param.paramName, param.argName, param.argType))

  def fromParameter[L <: LA, F[_]: Monad](
      protocolElems: List[StrictProtocolElems[L]],
      components: Tracker[Option[Components]]
  )(implicit
      Fw: FrameworkTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Tracker[Parameter] => F[LanguageParameter[L]] = { parameter =>
    import Fw._
    import Sc._
    import Cl._
    import Sw._

    def paramMeta(param: Tracker[Parameter]): F[(core.ResolvedType[L], Boolean)] = {
      def getDefault[U <: Parameter: Default.GetDefault](schema: Tracker[media.Schema[_]]): F[Option[L#Term]] =
        for {
          res <- schema
            .refine[F[Option[L#Term]]] { case x: media.StringSchema => x }(schema => Default(schema).extract[String].traverse(litString))
            .orRefine { case x: media.NumberSchema if x.getFormat() == "float" => x }(schema => Default(schema).extract[Float].traverse(litFloat))
            .orRefine { case x: media.NumberSchema => x }(schema => Default(schema).extract[Double].traverse(litDouble))
            .orRefine { case x: media.IntegerSchema if x.getFormat() == "int32" => x }(schema => Default(schema).extract[Int].traverse(litInt))
            .orRefine { case x: media.IntegerSchema => x }(schema => Default(schema).extract[Long].traverse(litLong))
            .orRefine { case x: media.BooleanSchema => x }(schema => Default(schema).extract[Boolean].traverse(litBoolean))
            .orRefineFallback(_ => Option.empty[L#Term].pure[F])
        } yield res

      def resolveParam(param: Tracker[Parameter]): F[(ResolvedType[L], Boolean)] =
        for {
          schema <- getParameterSchema(param, components).map(_.map {
            case SchemaLiteral(schema)               => schema
            case SchemaRef(SchemaLiteral(schema), _) => schema
          })
          prefixes <- vendorPrefixes()
          customTypeName = Tracker
            .cloneHistory(schema, CustomTypeName(schema.unwrapTracker, prefixes))
            .fold(Tracker.cloneHistory(param, CustomTypeName(param, prefixes)))(_.map(Option.apply))
          (declType, rawType) <- ModelResolver.determineTypeName[L, F](schema, customTypeName, components)
          defaultValue        <- getDefault(schema)
          res      = core.Resolved[L](declType, None, defaultValue, rawType)
          required = param.downField("required", _.getRequired()).unwrapTracker.getOrElse(false)
        } yield (res, required)

      def paramHasRefSchema(p: Parameter): Boolean = Option(p.getSchema).exists(s => Option(s.get$ref()).nonEmpty)

      param
        .refine[F[(core.ResolvedType[L], Boolean)]] { case r: Parameter if r.isRef => r }(r =>
          for {
            name <- getRefParameterRef(r)
            required = r.downField("required", _.getRequired()).unwrapTracker.getOrElse(false)
          } yield (core.Deferred(name.unwrapTracker), required)
        )
        .orRefine { case r: Parameter if paramHasRefSchema(r) => r }(r =>
          for {
            ref <- getSimpleRef(r.downField("schema", _.getSchema))
            required = r.downField("required", _.getRequired()).unwrapTracker.getOrElse(false)
          } yield (core.Deferred(ref), required)
        )
        .orRefine { case x: Parameter if x.isInBody => x }(param =>
          for {
            schema <- getParameterSchema(param, components).map(_.map {
              case SchemaLiteral(schema)               => schema
              case SchemaRef(SchemaLiteral(schema), _) => schema
            })
            resolved <- ModelResolver.modelMetaType[L, F](schema, components)
            required = param.downField("required", _.getRequired()).unwrapTracker.getOrElse(false)
          } yield (resolved, required)
        )
        .orRefine { case x: Parameter if x.isInHeader => x }(resolveParam)
        .orRefine { case x: Parameter if x.isInPath => x }(resolveParam)
        .orRefine { case x: Parameter if x.isInQuery => x }(resolveParam)
        .orRefine { case x: Parameter if x.isInCookies => x }(resolveParam)
        .orRefine { case x: Parameter if x.isInFormData => x }(resolveParam)
        .orRefineFallback(fallbackParameterHandler)
    }

    log.function(s"fromParameter")(
      for {
        _                                                           <- log.debug(parameter.unwrapTracker.showNotNull)
        (meta, required)                                            <- paramMeta(parameter)
        core.Resolved(paramType, _, rawDefaultType, reifiedRawType) <- core.ResolvedType.resolve[L, F](meta, protocolElems)

        declType <-
          if (!required) {
            liftOptionalType(paramType)
          } else {
            paramType.pure[F]
          }

        baseDefaultValue <- extractTypeName(paramType).flatMap(_.fold(rawDefaultType.traverse(_.pure[F])) { tpe =>
          protocolElems
            .flatTraverse {
              case x @ EnumDefinition(_, _tpeName, _, _, _, _) =>
                for {
                  areEqual <- typeNamesEqual(tpe, _tpeName)
                } yield if (areEqual) List(x) else List.empty[EnumDefinition[L]]
              case _ => List.empty[EnumDefinition[L]].pure[F]
            }
            .flatMap(_.headOption.fold[F[Option[L#Term]]](rawDefaultType.traverse(_.pure[F])) { x =>
              rawDefaultType.traverse(lookupEnumDefaultValue(tpe, _, x.elems).flatMap(widenTermSelect))
            })
        })

        defaultValue <-
          if (!required) {
            (baseDefaultValue.traverse(liftOptionalTerm), emptyOptionalTerm().map(Option.apply _)).mapN(_.orElse(_))
          } else {
            baseDefaultValue.pure[F]
          }

        name <- getParameterName(parameter)

        paramName     <- formatMethodArgName(name)
        paramTermName <- pureTermName(paramName)
        param         <- pureMethodParameter(paramTermName, declType, defaultValue)

        ftpe       <- fileType(None)
        isFileType <- typesEqual(paramType, ftpe)
      } yield new LanguageParameter[L](
        parameter.downField("in", _.getIn()).unwrapTracker,
        param,
        paramTermName,
        RawParameterName(name),
        declType,
        reifiedRawType,
        required,
        FileHashAlgorithm(parameter),
        isFileType
      )
    )
  }

  def fromParameters[L <: LA, F[_]: Monad](
      protocolElems: List[StrictProtocolElems[L]],
      components: Tracker[Option[Components]]
  )(implicit
      Fw: FrameworkTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): List[Tracker[Parameter]] => F[List[LanguageParameter[L]]] = { params =>
    import Sc._
    for {
      parameters <- params.traverse(fromParameter(protocolElems, components))
      counts     <- parameters.traverse(param => extractTermName(param.paramName)).map(_.groupBy(identity).view.mapValues(_.length).toMap)
      result <- parameters.traverse { param =>
        extractTermName(param.paramName).flatMap { name =>
          if (counts.getOrElse(name, 0) > 1) {
            pureTermName(param.argName.value).flatMap { escapedName =>
              alterMethodParameterName(param.param, escapedName).map { newParam =>
                new LanguageParameter[L](
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
          } else param.pure[F]
        }
      }
    } yield result
  }

  /** Create method parameters from Swagger's Path parameters list. Use Option for non-required parameters.
    * @param params
    * @return
    */
  def filterParams[L <: LA](params: List[LanguageParameter[L]]): String => List[LanguageParameter[L]] = { in =>
    params.filter(_.in == Some(in))
  }
}
