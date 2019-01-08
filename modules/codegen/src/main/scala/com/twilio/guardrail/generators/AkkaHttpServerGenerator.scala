package com.twilio.guardrail
package generators

import cats.arrow.FunctionK
import cats.data.{ NonEmptyList, OptionT }
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.guardrail.SwaggerUtil
import com.twilio.guardrail.extract.{ ScalaPackage, ScalaTracingLabel, ServerRawResponse }
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.terms.RouteMeta

import scala.collection.JavaConverters._
import scala.meta._
import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod

object AkkaHttpServerGenerator {
  object ServerTermInterp extends FunctionK[ServerTerm[ScalaLanguage, ?], Target] {
    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }
    def apply[T](term: ServerTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        for {
          _ <- Target.pure(())
          responseSuperType = Type.Name(s"${operationId}Response")
          responseSuperTerm = Term.Name(s"${operationId}Response")

          instances = responses.value
            .foldLeft[List[(Defn, Defn, Case)]](List.empty)({
              case (acc, resp) =>
                acc :+ ({
                  val statusCodeName = resp.statusCodeName
                  val statusCode     = q"StatusCodes.${statusCodeName}"
                  val valueType      = resp.value.map(_._1)
                  val responseTerm   = Term.Name(s"${operationId}Response${statusCodeName.value}")
                  val responseName   = Type.Name(s"${operationId}Response${statusCodeName.value}")
                  valueType.fold[(Defn, Defn, Case)](
                    (q"case object $responseTerm                      extends $responseSuperType($statusCode)",
                     q"def $statusCodeName: $responseSuperType = $responseTerm",
                     p"case r: $responseTerm.type => scala.concurrent.Future.successful(Marshalling.Opaque { () => HttpResponse(r.statusCode) } :: Nil)")
                  ) { valueType =>
                    (q"case class  $responseName(value: $valueType) extends $responseSuperType($statusCode)",
                     q"def $statusCodeName(value: $valueType): $responseSuperType = $responseTerm(value)",
                     p"case r@$responseTerm(value) => Marshal(value).to[ResponseEntity].map { entity => Marshalling.Opaque { () => HttpResponse(r.statusCode, entity=entity) } :: Nil }")
                  }
                })
            })

          (terms, aliases, marshallers) = instances.unzip3

          convenienceConstructors = aliases.flatMap({
            case q"def $name(value: $tpe): $_ = $_" => tpe.map { (_, name) }
            case _                                  => None
          })

          implicitHelpers = convenienceConstructors
            .groupBy(_._1)
            .flatMap({
              case (tpe, (_, name) :: Nil) =>
                Some(tpe -> name)
              case _ =>
                None
            })
            .toList
            .sortBy(_._2.value)
            .map {
              case (tpe, name) =>
                q"implicit def ${Term.Name(s"${name.value}Ev")}(value: ${tpe}): ${responseSuperType} = ${name}(value)"
            }

          companion = q"""
            object ${responseSuperTerm} {
              implicit val ${Pat
            .Var(Term.Name(s"${operationId}TRM"))}: ToResponseMarshaller[${responseSuperType}] = Marshaller { implicit ec => resp => ${Term
            .Name(s"${operationId}TR")}(resp) }
              implicit def ${Term
            .Name(s"${operationId}TR")}(value: ${responseSuperType})(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] =
                ${Term.Match(Term.Name("value"), marshallers)}

              def apply[T](value: T)(implicit ev: T => ${responseSuperType}): ${responseSuperType} = ev(value)

              ..${implicitHelpers}

              ..${aliases}
            }
          """

        } yield
          List[Defn](
            q"sealed abstract class ${responseSuperType}(val statusCode: StatusCode)"
          ) ++ terms ++ List[Defn](
            companion
          )

      case BuildTracingFields(operation, resourceName, tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"buildTracingFields(${operation}, ${resourceName}, ${tracing})")
          res <- if (tracing) {
            for {
              operationId <- Target.fromOption(Option(operation.getOperationId())
                                                 .map(splitOperationParts)
                                                 .map(_._2),
                                               "Missing operationId")
              label <- Target.fromOption(
                ScalaTracingLabel(operation)
                  .map(Lit.String(_))
                  .orElse(resourceName.lastOption.map(clientName => Lit.String(s"${clientName}:${operationId}"))),
                "Missing client name"
              )
            } yield Some(TracingField[ScalaLanguage](ScalaParameter.fromParam(param"traceBuilder: TraceBuilder"), q"""trace(${label})"""))
          } else Target.pure(None)
        } yield res

      case GenerateRoutes(resourceName, basePath, routes, protocolElems) =>
        for {
          renderedRoutes <- routes.traverse {
            case (operationId, tracingFields, sr @ RouteMeta(path, method, operation), parameters, responses) =>
              for {
                rendered <- generateRoute(resourceName, basePath, sr, tracingFields, parameters, responses)
              } yield rendered
          }
          routeTerms = renderedRoutes.map(_.route)
          combinedRouteTerms <- combineRouteTerms(routeTerms)
          methodSigs = renderedRoutes.map(_.methodSig)
        } yield {
          RenderedRoutes[ScalaLanguage](
            combinedRouteTerms,
            methodSigs,
            renderedRoutes.flatMap(_.supportDefinitions),
            renderedRoutes.flatMap(_.handlerDefinitions)
          )
        }

      case RenderHandler(handlerName, methodSigs, handlerDefinitions) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"renderHandler(${handlerName}, ${methodSigs}")
        } yield q"""
          trait ${Type.Name(handlerName)} {
            ..${methodSigs ++ handlerDefinitions}
          }
        """

      case GetExtraRouteParams(tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"getExtraRouteParams(${tracing})")
          res <- if (tracing) {
            Target.pure(List(param"""trace: String => Directive1[TraceBuilder]"""))
          } else Target.pure(List.empty)
        } yield res

      case RenderClass(resourceName, handlerName, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"renderClass(${resourceName}, ${handlerName}, <combinedRouteTerms>, ${extraRouteParams})")
          routesParams = List(param"handler: ${Type.Name(handlerName)}") ++ extraRouteParams
        } yield List(q"""
          object ${Term.Name(resourceName)} {
            def discardEntity(implicit mat: akka.stream.Materializer): Directive0 = extractRequest.flatMap({ req => req.discardEntityBytes().future; Directive.Empty })

            ..${supportDefinitions};
            def routes(..${routesParams})(implicit mat: akka.stream.Materializer): Route = {
              ${combinedRouteTerms}
            }

            ..${responseDefinitions}
          }
        """)

      case GetExtraImports(tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"getExtraImports(${tracing})")
        } yield {
          List(
            if (tracing) Option(q"import akka.http.scaladsl.server.Directive1") else None,
            Option(q"import scala.language.higherKinds")
          ).flatten
        }
    }

    def httpMethodToAkka(method: HttpMethod): Target[Term] = method match {
      case HttpMethod.DELETE  => Target.pure(q"delete")
      case HttpMethod.GET     => Target.pure(q"get")
      case HttpMethod.PATCH   => Target.pure(q"patch")
      case HttpMethod.POST    => Target.pure(q"post")
      case HttpMethod.PUT     => Target.pure(q"put")
      case HttpMethod.OPTIONS => Target.pure(q"options")
      case other              => Target.raiseError(s"Unknown method: ${other}")
    }

    def pathStrToAkka(basePath: Option[String], path: String, pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[Term] = {

      def addTrailingSlashMatcher(trailingSlash: Boolean, term: Term.Apply): Term =
        if (trailingSlash)
          q"${term.copy(fun = Term.Name("pathPrefix"))} & pathEndOrSingleSlash"
        else term

      (basePath.getOrElse("") + path).stripPrefix("/") match {
        case "" => Target.pure(q"pathEndOrSingleSlash")
        case path =>
          for {
            pathDirective <- SwaggerUtil.paths
              .generateUrlAkkaPathExtractors(path, pathArgs)
          } yield pathDirective
      }
    }

    def directivesFromParams(
        required: Term => Type => Target[Term.Apply],
        multi: Term => Type => Target[Term.Apply],
        multiOpt: Term => Type => Target[Term.Apply],
        optional: Term => Type => Target[Term.Apply]
    )(params: List[ScalaParameter[ScalaLanguage]]): Target[Option[Term]] =
      for {
        directives <- params.traverse {
          case ScalaParameter(_, param, _, argName, argType) =>
            param match {
              case param"$_: Option[Iterable[$tpe]]" =>
                multiOpt(argName.toLit)(tpe)
              case param"$_: Option[Iterable[$tpe]] = $_" =>
                multiOpt(argName.toLit)(tpe)
              case param"$_: Option[$tpe]" =>
                optional(argName.toLit)(tpe)
              case param"$_: Option[$tpe] = $_" =>
                optional(argName.toLit)(tpe)
              case param"$_: Iterable[$tpe]" =>
                multi(argName.toLit)(tpe)
              case param"$_: Iterable[$tpe] = $_" =>
                multi(argName.toLit)(tpe)
              case _ => required(argName.toLit)(argType)
            }
        }
      } yield
        directives match {
          case Nil => Option.empty
          case x :: xs =>
            Some(xs.foldLeft[Term](x) { case (a, n) => q"${a} & ${n}" })
        }

    def bodyToAkka(operationId: String, body: Option[ScalaParameter[ScalaLanguage]]): Target[Option[Term]] =
      Target.pure(
        body.map {
          case ScalaParameter(_, _, _, _, argType) =>
            q"entity(as[${argType}](${Term.Name(s"${operationId}Decoder")}))"
        }
      )

    def headersToAkka: List[ScalaParameter[ScalaLanguage]] => Target[Option[Term]] =
      directivesFromParams(
        arg => {
          case t"String" => Target.pure(q"headerValueByName(${arg})")
          case tpe =>
            Target.pure(
              q"""
                headerValueByName(${arg})
                  .flatMap(str =>
                    onComplete(Unmarshal(str).to[${tpe}])
                      .flatMap[Tuple1[${tpe}]]({
                        case Failure(e) => reject(MalformedHeaderRejection(${arg}, e.getMessage, Some(e)))
                        case Success(x) => provide(x)
                      }))
              """
            )
        },
        arg => tpe => Target.raiseError(s"Unsupported Iterable[${arg}]"),
        arg => tpe => Target.raiseError(s"Unsupported Option[Iterable[${arg}]]"),
        arg => {
          case t"String" => Target.pure(q"optionalHeaderValueByName(${arg})")
          case tpe =>
            Target.pure(
              q"""
                optionalHeaderValueByName(${arg})
                  .flatMap(
                    _.fold[Directive1[Option[${tpe}]]](provide(Option.empty[${tpe}]))(str =>
                      onComplete(Unmarshal(str).to[${tpe}]).flatMap[Tuple1[Option[${tpe}]]]({
                        case Failure(e) => reject(MalformedHeaderRejection(${arg}, e.getMessage, Some(e)))
                        case Success(x) => provide(Option(x))
                      })))
              """
            )
        }
      ) _

    def qsToAkka: List[ScalaParameter[ScalaLanguage]] => Target[Option[Term]] =
      directivesFromParams(
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}])"),
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}].*)"),
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}].*).map(Option.apply _)"),
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}].?)")
      ) _

    def formToAkka: List[ScalaParameter[ScalaLanguage]] => Target[Option[Term]] =
      directivesFromParams(
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}])"),
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}].*)"),
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}].*).map(Option.apply _)"),
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}].?)")
      ) _

    def asyncFormToAkka(operationId: String): List[ScalaParameter[ScalaLanguage]] => Target[(Option[Term], List[Stat])] = { params =>
      class Binding(value: String) {
        def toPat: Pat        = if (value.nonEmpty && ('A'.to('Z').contains(value(0)))) toTerm else toVar
        def toVar: Pat.Var    = Pat.Var(toTerm)
        def toTerm: Term.Name = Term.Name(value)
        def toType: Type.Name = Type.Name(value)

        override def toString(): String = s"Binding($value)"
      }
      (for {
        params <- NonEmptyList.fromList(params)
      } yield {
        val partsTerm = Term.Name(s"${operationId}Parts")
        val value = params
          .map({
            case rawParameter @ ScalaParameter(a, param, paramName, argName, argType) =>
              val containerName    = new Binding(paramName.value)
              val unmarshallerName = new Binding(s"Unmarshal${paramName.value}Part")
              val binding          = new Binding(paramName.value)
              val collected        = new Binding(s"${paramName.value}O")

              val isFile: Boolean = rawParameter.isFile
              val (isOptional, realType): (Boolean, Type) = argType match {
                case t"Option[$x]" => (true, x)
                case x             => (false, x)
              }
              val (unmarshaller, caseMatch, grabHead) = isFile match {
                case true =>
                  (
                    q"""
                        val ${unmarshallerName.toVar}: Unmarshaller[Multipart.FormData.BodyPart, ${Type.Select(partsTerm, containerName.toType)}] = (
                            handler.${Term.Name(s"${operationId}UnmarshalToFile")}[${rawParameter.hashAlgorithm.fold(t"Option")(Function.const(t"Id"))}](
                                ${rawParameter.hashAlgorithm.fold[Term](q"None")(x => Lit.String(x))}, handler.${Term
                      .Name(s"${operationId}MapFileField")}(_, _, _)
                              ).map({ case (v1, v2, v3, v4) =>
                                ${Term.Select(partsTerm, containerName.toTerm)}((..${List(q"v1", q"v2", q"v3") ++ rawParameter.hashAlgorithm
                      .map(Function.const(q"v4"))}))
                              })
                            )
                      """,
                    Case(
                      argName.toLit,
                      None,
                      q"""
                          SafeUnmarshaller(AccumulatingUnmarshaller(fileReferences, ${unmarshallerName.toTerm})(_.value._1)).apply(part)
                      """
                    ),
                    q"""
                        val ${collected.toVar} = successes.collectFirst(${Term.PartialFunction(
                      List(
                        Case(
                          Pat.Extract(Term.Select(partsTerm, containerName.toTerm),
                                      List(p"((..${List(p"v1", p"v2", p"v3") ++ rawParameter.hashAlgorithm.map(Function.const(p"v4"))}))")),
                          None,
                          q"(..${List(q"v1", q"v2", q"v3") ++ rawParameter.hashAlgorithm.map(Function.const(q"v4"))})"
                        )
                      )
                    )})
                      """
                  )
                case false =>
                  (
                    q"""
                        val ${unmarshallerName.toVar}: Unmarshaller[Multipart.FormData.BodyPart, ${Type
                      .Select(partsTerm, containerName.toType)}] = Unmarshaller { implicit executionContext => part =>
                          val json: Unmarshaller[Multipart.FormData.BodyPart, ${realType}] = MFDBPviaFSU(jsonEntityUnmarshaller[${realType}])
                          val string: Unmarshaller[Multipart.FormData.BodyPart, ${realType}] = MFDBPviaFSU(BPEviaFSU(jsonDecoderUnmarshaller))
                          Unmarshaller.firstOf(json, string).apply(part).map(${Term.Select(partsTerm, containerName.toTerm)}.apply)
                        }
                      """,
                    Case(argName.toLit, None, q"""
                          SafeUnmarshaller(${unmarshallerName.toTerm}).apply(part)
                      """),
                    q"""
                        val ${collected.toVar} = successes.collectFirst(${Term.PartialFunction(
                      List(Case(Pat.Extract(Term.Select(partsTerm, containerName.toTerm), List(Pat.Var(Term.Name("v1")))), None, Term.Name("v1")))
                    )})
                      """
                  )
              }

              val partContainer = q"case class ${containerName.toType}(..${List(param"value: ${realType}")}) extends Part"

              val unpacker = if (isOptional) {
                enumerator""" ${binding.toVar} <- Either.right[MissingFormFieldRejection, ${argType}](${collected.toTerm}) """
              } else {
                enumerator""" ${binding.toVar} <- ${collected.toTerm}.toRight(MissingFormFieldRejection(${argName.toLit})) """
              }

              (partContainer, unmarshaller, caseMatch, (binding, collected), unpacker, argType, grabHead)
          })
          .unzip7

        val (partContainers, unmarshallers, matchers, _terms, unpacks, termTypes, grabHeads) = value
        val (termPatterns, optionalTermPatterns)                                             = _terms.unzip
        val optionalTuple = optionalTermPatterns match {
          case binding :: Nil => p"Tuple1(${binding.toPat})"
          case xs             => p"(..${xs.map(_.toPat)})"
        }

        val _trait              = q"sealed trait Part"
        val ignoredPart         = q"case class IgnoredPart(unit: Unit) extends Part"
        val ignoredUnmarshaller = p"""
          case _ =>
            SafeUnmarshaller(
            implicitly[Unmarshaller[Multipart.FormData.BodyPart, Unit]]
                                                        .map(${partsTerm}.IgnoredPart.apply(_))
          ).apply(part)
        """

        val fieldNames = q"""Set[String](..${params.toList.map(_.argName.toLit)})"""
        val optionalTypes = termTypes.map({
          case x @ t"Option[$_]" => x
          case x                 => t"Option[$x]"
        }) match {
          case tpe :: Nil => t"Tuple1[${tpe}]"
          case xs         => t"(..${xs})"
        }
        val unpackedTypes = termTypes match {
          case tpe :: Nil => t"Tuple1[${tpe}]"
          case xs         => t"(..${xs})"
        }

        val allCases: List[Case] = matchers ++ List(ignoredUnmarshaller)

        val handlerDefinitions = List(
          q"""
          def ${Term.Name(s"${operationId}MapFileField")}(fieldName: String, fileName: Option[String], contentType: ContentType): File
          """,
          q"""
            def ${Term
            .Name(s"${operationId}UnmarshalToFile")}[F[_]: Functor](hashType: F[String], destFn: (String, Option[String], ContentType) => File)(implicit mat: Materializer): Unmarshaller[Multipart.FormData.BodyPart, (File, Option[String], ContentType, F[String])] = Unmarshaller { implicit executionContext => part =>
              val dest = destFn(part.name, part.filename, part.entity.contentType)
              val messageDigest = hashType.map(MessageDigest.getInstance(_))
              val fileSink: Sink[ByteString,Future[IOResult]] = FileIO.toPath(dest.toPath).contramap[ByteString] { chunk => val _ = messageDigest.map(_.update(chunk.toArray[Byte])); chunk }

              part.entity.dataBytes.toMat(fileSink)(Keep.right).run()
                .transform({
                  case IOResult(_, Success(_)) =>
                    val hash = messageDigest.map(md => javax.xml.bind.DatatypeConverter.printHexBinary(md.digest()).toLowerCase(java.util.Locale.US))
                    (dest, part.filename, part.entity.contentType, hash)
                  case IOResult(_, Failure(t)) =>
                    dest.delete()
                    throw t
                }, { case t =>
                  dest.delete()
                  t
                })
            }
          """
        )

        val directive: Term = (
          q"""
          ({
            object ${partsTerm} {
              ..${List(
            _trait,
            ignoredPart
          ) ++ partContainers}
            }

            ..${unmarshallers};

            (extractExecutionContext.flatMap { implicit executionContext =>
              extractMaterializer.flatMap { implicit mat =>
                val fileReferences = new AtomicReference(List.empty[File])
                (
                  extractSettings.flatMap({ settings =>
                    handleExceptions(ExceptionHandler {
                      case EntityStreamSizeException(limit, contentLength) ⇒
                        fileReferences.get().foreach(_.delete())
                        val summary = contentLength match {
                          case Some(cl) => s"Request Content-Length of $$cl bytes exceeds the configured limit of $$limit bytes"
                          case None     => s"Aggregated data length of request entity exceeds the configured limit of $$limit bytes"
                        }
                        val info = new ErrorInfo(summary, "Consider increasing the value of akka.http.server.parsing.max-content-length")
                        val status = StatusCodes.RequestEntityTooLarge
                        val msg = if (settings.verboseErrorMessages) info.formatPretty else info.summary
                        complete(HttpResponse(status, entity = msg))
                      case e: Throwable =>
                        fileReferences.get().foreach(_.delete())
                        throw e
                    })
                  }) & handleRejections({ rejections: scala.collection.immutable.Seq[Rejection] =>
                    fileReferences.get().foreach(_.delete())
                    None
                  }) & mapResponse({ resp =>
                    fileReferences.get().foreach(_.delete())
                    resp
                  }) & entity(as[Multipart.FormData])
                ).flatMap { formData =>
                  val collectedPartsF: Future[Either[Throwable, ${optionalTypes}]] = for {
                    results <- formData.parts
                      .mapConcat({ part =>
                        if (${fieldNames}.contains(part.name)) part :: Nil
                        else {
                          part.entity.discardBytes()
                          Nil
                        }
                      }).mapAsync(1)({ part =>
                        ${Term.Match(q"part.name", allCases)}
                      }).toMat(Sink.seq[Either[Throwable, ${Type.Select(partsTerm, Type.Name("Part"))}]])(Keep.right).run()
                    } yield {
                      results.toList.sequence.map({ successes =>
                        ..${grabHeads}

                        ${optionalTermPatterns.map(_.toTerm) match {
            case term :: Nil => q"Tuple1(${term})"
            case xs          => q"(..${xs})"
          }}
                      })
                    }

                  onSuccess(collectedPartsF)
                }
              }
            }).flatMap(_.fold(t => throw t, ${Term.PartialFunction(
            List(
              Case(
                optionalTuple,
                None,
                q"""
              val maybe: Either[Rejection, ${unpackedTypes}] = for {
                ..${unpacks}
              } yield {
                ${termPatterns.map(_.toTerm) match {
                  case term :: Nil => q"Tuple1(${term})"
                  case xs          => q"(..${xs})"
                }}
              }
              maybe.fold(reject(_), tprovide(_))
            """
              )
            )
          )}))
          }: Directive[${unpackedTypes}])
        """
        )

        (directive, handlerDefinitions)
      }).fold(Target.pure((Option.empty[Term], List.empty[Stat])))({ case (v1, v2) => Target.pure((Option(v1), v2)) })
    }

    case class RenderedRoute(route: Term, methodSig: Decl.Def, supportDefinitions: List[Defn], handlerDefinitions: List[Stat])

    def generateRoute(resourceName: String,
                      basePath: Option[String],
                      route: RouteMeta,
                      tracingFields: Option[TracingField[ScalaLanguage]],
                      parameters: ScalaParameters[ScalaLanguage],
                      responses: Responses[ScalaLanguage]): Target[RenderedRoute] =
      // Generate the pair of the Handler method and the actual call to `complete(...)`
      for {
        _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"generateRoute(${resourceName}, ${basePath}, ${route}, ${tracingFields})")
        RouteMeta(path, method, operation) = route
        operationId <- Target.fromOption(Option(operation.getOperationId())
                                           .map(splitOperationParts)
                                           .map(_._2),
                                         "Missing operationId")

        // special-case file upload stuff
        formArgs = parameters.formParams.map({ x =>
          x.withType(
            if (x.isFile) {
              val fileParams = List(t"File", t"Option[String]", t"ContentType") ++ x.hashAlgorithm.map(Function.const(t"String"))
              if (x.required) {
                t"(..${fileParams})"
              } else {
                t"Option[(..${fileParams})]"
              }
            } else {
              x.argType
            }
          )
        })
        headerArgs = parameters.headerParams
        pathArgs   = parameters.pathParams
        qsArgs     = parameters.queryStringParams
        bodyArgs   = parameters.bodyParams

        akkaMethod <- httpMethodToAkka(method)
        akkaPath   <- pathStrToAkka(basePath, path, pathArgs)
        akkaQs     <- qsToAkka(qsArgs)
        akkaBody   <- bodyToAkka(operationId, bodyArgs)
        asyncFormProcessing = formArgs.exists(_.isFile)
        akkaForm_ <- if (asyncFormProcessing) { asyncFormToAkka(operationId)(formArgs) } else { formToAkka(formArgs).map((_, List.empty[Defn])) }
        (akkaForm, handlerDefinitions) = akkaForm_
        akkaHeaders <- headersToAkka(headerArgs)
      } yield {
        val (responseCompanionTerm, responseCompanionType) =
          (Term.Name(s"${operationId}Response"), Type.Name(s"${operationId}Response"))
        val responseType = ServerRawResponse(operation)
          .filter(_ == true)
          .fold[Type](t"${Term.Name(resourceName)}.${responseCompanionType}")(Function.const(t"HttpResponse"))
        val orderedParameters
          : List[List[ScalaParameter[ScalaLanguage]]] = List((pathArgs ++ qsArgs ++ bodyArgs ++ formArgs ++ headerArgs).toList) ++ tracingFields
          .map(_.param)
          .map(List(_))

        val entityProcessor = akkaBody.orElse(akkaForm).getOrElse(q"discardEntity")
        val fullRouteMatcher =
          List[Option[Term]](Some(akkaMethod), Some(akkaPath), akkaQs, Some(entityProcessor), akkaHeaders, tracingFields.map(_.term)).flatten.reduceLeft {
            (a, n) =>
              q"${a} & ${n}"
          }
        val handlerCallArgs: List[List[Term.Name]] = List(List(responseCompanionTerm)) ++ orderedParameters.map(_.map(_.paramName))
        val fullRoute: Term.Apply = orderedParameters match {
          case List(List()) =>
            q"""
              ${fullRouteMatcher} {
                complete(handler.${Term
              .Name(operationId)}(...${handlerCallArgs}))
              }
              """
          case params =>
            q"""
              ${fullRouteMatcher} { (..${params.flatten.map(p => param"${p.paramName}")}) =>
                complete(handler.${Term
              .Name(operationId)}(...${handlerCallArgs}))
              }
              """
        }

        val respond: List[List[Term.Param]] = List(List(param"respond: ${Term.Name(resourceName)}.${responseCompanionTerm}.type"))

        val params: List[List[Term.Param]] = respond ++ orderedParameters.map(
          _.map(
            scalaParam =>
              scalaParam.param.copy(
                decltpe =
                  (
                    if (scalaParam.isFile) {
                      val fileParams = List(t"File", t"Option[String]", t"ContentType") ++ scalaParam.hashAlgorithm.map(Function.const(t"String"))
                      if (scalaParam.required) {
                        Some(t"(..${fileParams})")
                      } else {
                        Some(t"Option[(..${fileParams})]")
                      }
                    } else {
                      scalaParam.param.decltpe
                    }
                  )
            )
          )
        )

        val consumes = Option(operation.getRequestBody).flatMap(x => Option(x.getContent)).fold(List.empty[String])(_.asScala.keys.toList)
        RenderedRoute(
          fullRoute,
          q"""
              def ${Term
            .Name(operationId)}(...${params}): scala.concurrent.Future[${responseType}]
            """,
          generateCodecs(operationId, bodyArgs, responses, consumes),
          handlerDefinitions
        )
      }

    def combineRouteTerms(terms: List[Term]): Target[Term] =
      for {
        _      <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"combineRouteTerms(<${terms.length} routes>)")
        routes <- Target.fromOption(NonEmptyList.fromList(terms), "Generated no routes, no source to generate")
        _      <- routes.traverse(route => Target.log.debug("AkkaHttpServerGenerator", "server", "combineRouteTerms")(route.toString))
      } yield routes.tail.foldLeft(routes.head) { case (a, n) => q"${a} ~ ${n}" }

    def generateCodecs(operationId: String,
                       bodyArgs: Option[ScalaParameter[ScalaLanguage]],
                       responses: Responses[ScalaLanguage],
                       consumes: Seq[String]): List[Defn.Val] =
      generateDecoders(operationId, bodyArgs, consumes)

    def generateDecoders(operationId: String, bodyArgs: Option[ScalaParameter[ScalaLanguage]], consumes: Seq[String]): List[Defn.Val] =
      bodyArgs.toList.flatMap {
        case ScalaParameter(_, _, _, _, argType) =>
          val (decoder, baseType) = AkkaHttpHelper.generateDecoder(argType, consumes)
          List(
            q"""
              val ${Pat.Typed(Pat.Var(Term.Name(s"${operationId}Decoder")), t"FromRequestUnmarshaller[$baseType]")} = {
                val extractEntity = implicitly[Unmarshaller[HttpMessage, HttpEntity]]
                val unmarshalEntity = ${decoder}
                extractEntity.andThen(unmarshalEntity)
              }
            """
          )
      }
  }
}
