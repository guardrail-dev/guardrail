package com.twilio.guardrail
package generators
package Java

import _root_.io.swagger.v3.oas.models.media._
import cats.data.NonEmptyList
import cats.implicits._
import cats.~>
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type }
import com.twilio.guardrail.extract.{ Default, ScalaEmptyIsNull }
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.generators.syntax.RichString
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.protocol._
import java.util.Locale
import scala.collection.JavaConverters._
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.{ ImportDeclaration, Node, NodeList }
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.Modifier.{ ABSTRACT, FINAL, PRIVATE, PUBLIC, STATIC }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import java.util
import scala.language.existentials
import scala.util.Try

object JacksonGenerator {
  private case class ParameterTerm(propertyName: String, parameterName: String, fieldType: Type, parameterType: Type, defaultValue: Option[Expression])

  // returns a tuple of (requiredTerms, optionalTerms)
  // note that required terms _that have a default value_ are conceptually optional.
  private def sortParams(params: List[ProtocolParameter[JavaLanguage]]): (List[ParameterTerm], List[ParameterTerm]) = {
    def defaultValueToExpression(defaultValue: Option[Node]): Option[Expression] = defaultValue match {
      case Some(expr: Expression) => Some(expr)
      case _                      => None
    }

    params
      .map({
        case ProtocolParameter(term, name, _, _, _, selfDefaultValue) =>
          val parameterType = if (term.getType.isOptional) {
            term.getType.containedType.unbox
          } else {
            term.getType
          }
          val defaultValue = defaultValueToExpression(selfDefaultValue)

          ParameterTerm(name, term.getNameAsString, term.getType, parameterType, defaultValue)
      })
      .partition(
        pt => !pt.fieldType.isOptional && pt.defaultValue.isEmpty
      )
  }

  private def addParents(cls: ClassOrInterfaceDeclaration, parentOpt: Option[SuperClass[JavaLanguage]]): Unit =
    parentOpt.foreach({ parent =>
      val directParent = JavaParser.parseClassOrInterfaceType(parent.clsName)
      val otherParents = parent.interfaces.map(JavaParser.parseClassOrInterfaceType)
      cls.setExtendedTypes(
        new NodeList((directParent +: otherParents): _*)
      )
    })

  private def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[JavaLanguage]])(f: Type => Target[Type]): Option[Target[Type]] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)

  private val HASH_MAP_TYPE_DIAMONDED = JavaParser
    .parseClassOrInterfaceType("java.util.HashMap")
    .setTypeArguments(new NodeList[Type])
  private val ARRAY_LIST_TYPE_DIAMONDED = JavaParser
    .parseClassOrInterfaceType("java.util.ArrayList")
    .setTypeArguments(new NodeList[Type])

  object EnumProtocolTermInterp extends (EnumProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: EnumProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractEnum(swagger) =>
        val enumEntries: Option[List[String]] = swagger match {
          case x: StringSchema =>
            Option[java.util.List[String]](x.getEnum()).map(_.asScala.toList)
          case x =>
            Option[java.util.List[_]](x.getEnum()).map(_.asScala.toList.map(_.toString()))
        }
        Target.pure(Either.fromOption(enumEntries, "Model has no enumerations"))

      case RenderMembers(clsName, elems) =>
        Target.pure(None)

      case EncodeEnum(clsName) =>
        Target.pure(None)

      case DecodeEnum(clsName) =>
        Target.pure(None)

      case RenderClass(clsName, tpe, elems) =>
        val enumType = JavaParser.parseType(clsName)

        val enumDefns = elems.map {
          case (value, termName, _) =>
            new EnumConstantDeclaration(
              new NodeList(),
              new SimpleName(termName.getIdentifier.toSnakeCase.toUpperCase(Locale.US)),
              new NodeList(new StringLiteralExpr(value)),
              new NodeList()
            )
        }

        val nameField = new FieldDeclaration(
          util.EnumSet.of(PRIVATE, FINAL),
          new VariableDeclarator(STRING_TYPE, "name")
        )

        val constructor = new ConstructorDeclaration(util.EnumSet.of(PRIVATE), clsName)
        constructor.addParameter(new Parameter(util.EnumSet.of(FINAL), STRING_TYPE, new SimpleName("name")))
        constructor.setBody(
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(
                new AssignExpr(
                  new FieldAccessExpr(new ThisExpr, "name"),
                  new NameExpr("name"),
                  AssignExpr.Operator.ASSIGN
                )
              )
            )
          )
        )

        val getNameMethod = new MethodDeclaration(
          util.EnumSet.of(PUBLIC),
          STRING_TYPE,
          "getName"
        )
        getNameMethod.addMarkerAnnotation("JsonValue")
        getNameMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(new FieldAccessExpr(new ThisExpr, "name"))
            )
          )
        )

        val parseMethod = new MethodDeclaration(
          util.EnumSet.of(PUBLIC, STATIC),
          enumType,
          "parse"
        )
        parseMethod.addMarkerAnnotation("JsonCreator")
        parseMethod.addParameter(new Parameter(util.EnumSet.of(FINAL), STRING_TYPE, new SimpleName("name")))
        parseMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ForEachStmt(
                new VariableDeclarationExpr(new VariableDeclarator(enumType, "value"), FINAL),
                new MethodCallExpr("values"),
                new BlockStmt(
                  new NodeList(
                    new IfStmt(
                      new MethodCallExpr("value.name.equals", new NameExpr("name")),
                      new ReturnStmt(new NameExpr("value")),
                      null
                    )
                  )
                )
              ),
              new ThrowStmt(
                new ObjectCreationExpr(
                  null,
                  JavaParser.parseClassOrInterfaceType("IllegalArgumentException"),
                  new NodeList(
                    new BinaryExpr(
                      new BinaryExpr(new StringLiteralExpr("Name '"), new NameExpr("name"), BinaryExpr.Operator.PLUS),
                      new StringLiteralExpr(s"' is not valid for enum '${clsName}'"),
                      BinaryExpr.Operator.PLUS
                    )
                  )
                )
              )
            )
          )
        )

        val staticInitializer = new InitializerDeclaration(
          true,
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(
                new MethodCallExpr(
                  new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
                  "register",
                  new NodeList[Expression](
                    new ClassExpr(JavaParser.parseClassOrInterfaceType(clsName)),
                    new MethodReferenceExpr(new NameExpr(clsName), null, "getName")
                  )
                )
              )
            )
          )
        )

        val enumClass = new EnumDeclaration(
          util.EnumSet.of(PUBLIC),
          new NodeList(),
          new SimpleName(clsName),
          new NodeList(),
          new NodeList(enumDefns: _*),
          new NodeList(
            staticInitializer,
            nameField,
            constructor,
            getNameMethod,
            parseMethod
          )
        )

        Target.pure(enumClass)

      case RenderStaticDefns(clsName, members, accessors, encoder, decoder) =>
        for {
          extraImports <- List(
            "com.fasterxml.jackson.annotation.JsonCreator",
            "com.fasterxml.jackson.annotation.JsonValue"
          ).traverse(safeParseRawImport)
        } yield
          StaticDefns[JavaLanguage](
            className = clsName,
            extraImports = extraImports,
            definitions = List.empty
          )

      case BuildAccessor(clsName, termName) =>
        Target.pure(new Name(s"${clsName}.${termName}"))
    }
  }

  object ModelProtocolTermInterp extends (ModelProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ModelProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        (swagger match {
          case m: ObjectSchema => Target.pure(Option(m.getProperties))
          case comp: ComposedSchema =>
            Target.pure(Option(comp.getAllOf).flatMap(_.asScala.toList.lastOption).flatMap(prop => Option(prop.getProperties)))
          case comp: Schema[_] if Option(comp.get$ref).isDefined =>
            Target.raiseError(s"Attempted to extractProperties for a ${comp.getClass()}, unsure what to do here")
          case _ => Target.pure(None)
        }).map(_.map(_.asScala.toList).toList.flatten)

      case TransformProperty(clsName, name, property, meta, needCamelSnakeConversion, concreteTypes, isRequired) =>
        Target.log.function("transformProperty") {
          for {
            defaultValue <- property match {
              case _: MapSchema =>
                Target.pure(Option(new ObjectCreationExpr(null, HASH_MAP_TYPE_DIAMONDED, new NodeList())).map(x => x: Expression))
              case _: ArraySchema =>
                Target.pure(Option(new ObjectCreationExpr(null, ARRAY_LIST_TYPE_DIAMONDED, new NodeList())).map(x => x: Expression))
              case p: BooleanSchema =>
                Default(p).extract[Boolean].traverse(x => Target.pure(new BooleanLiteralExpr(x)))
              case p: NumberSchema if p.getFormat == "double" =>
                Default(p).extract[Double].traverse(x => Target.pure(new DoubleLiteralExpr(x)))
              case p: NumberSchema if p.getFormat == "float" =>
                Default(p).extract[Float].traverse(x => Target.pure(new DoubleLiteralExpr(x)))
              case p: IntegerSchema if p.getFormat == "int32" =>
                Default(p).extract[Int].traverse(x => Target.pure(new IntegerLiteralExpr(x)))
              case p: IntegerSchema if p.getFormat == "int64" =>
                Default(p).extract[Long].traverse(x => Target.pure(new LongLiteralExpr(x)))
              case p: StringSchema =>
                Default(p)
                  .extract[String]
                  .traverse(x => Target.fromOption(Try(new StringLiteralExpr(x)).toOption, s"Default string literal for '${p.getTitle}' is null"))
              case _ =>
                Target.pure(None)
            }

            readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
            emptyToNull = (property match {
              case d: DateSchema      => ScalaEmptyIsNull(d)
              case dt: DateTimeSchema => ScalaEmptyIsNull(dt)
              case s: StringSchema    => ScalaEmptyIsNull(s)
              case _                  => None
            }).getOrElse(EmptyIsEmpty)

            tpeClassDep <- meta match {
              case SwaggerUtil.Resolved(declType, classDep, _) =>
                Target.pure((declType, classDep))
              case SwaggerUtil.Deferred(tpeName) =>
                val tpe = concreteTypes.find(_.clsName == tpeName).map(x => Target.pure(x.tpe)).getOrElse {
                  println(s"Unable to find definition for ${tpeName}, just inlining")
                  safeParseType(tpeName)
                }
                tpe.map((_, Option.empty))
              case SwaggerUtil.DeferredArray(tpeName) =>
                safeParseType(s"java.util.List<${tpeName}>").map((_, Option.empty))
              case SwaggerUtil.DeferredMap(tpeName) =>
                safeParseType(s"java.util.List<${tpeName}>").map((_, Option.empty))
            }
            (tpe, classDep) = tpeClassDep

            argName = if (needCamelSnakeConversion) name.toCamelCase else name
            _declDefaultPair <- Option(isRequired)
              .filterNot(_ == false)
              .fold[Target[(Type, Option[Expression])]](
                (
                  safeParseType(s"java.util.Optional<${tpe}>"),
                  Target.pure(
                    Option(
                      defaultValue
                        .fold(
                          new MethodCallExpr(new NameExpr(s"java.util.Optional"), "empty")
                        )(t => new MethodCallExpr("java.util.Optional.of", t))
                    )
                  )
                ).mapN((_, _))
              )(Function.const(Target.pure((tpe, defaultValue))) _)
            (finalDeclType, finalDefaultValue) = _declDefaultPair
            term <- safeParseParameter(s"final ${finalDeclType} ${argName.escapeReservedWord}")
            dep = classDep.filterNot(_.value == clsName) // Filter out our own class name
          } yield ProtocolParameter[JavaLanguage](term, name, dep, readOnlyKey, emptyToNull, defaultValue)
        }

      case RenderDTOClass(clsName, selfParams, parents) =>
        val dtoClassType   = JavaParser.parseClassOrInterfaceType(clsName)
        val discriminators = parents.flatMap(_.discriminators)
        val parentOpt      = parents.headOption
        val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(
          param => discriminators.contains(param.term.getName().getId())
        )
        val (requiredTerms, optionalTerms) = sortParams(params)
        val terms                          = requiredTerms ++ optionalTerms

        val dtoClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, clsName)
        dtoClass.addAnnotation(
          new NormalAnnotationExpr(
            new Name("JsonIgnoreProperties"),
            new NodeList(
              new MemberValuePair(
                "ignoreUnknown",
                new BooleanLiteralExpr(true)
              )
            )
          )
        )

        addParents(dtoClass, parentOpt)

        discriminators.foreach({ discriminator =>
          val field = dtoClass.addFieldWithInitializer(STRING_TYPE, discriminator, new StringLiteralExpr(clsName), PRIVATE, FINAL)
          field.addAnnotation(
            new NormalAnnotationExpr(
              new Name("JsonProperty"),
              new NodeList(
                new MemberValuePair("value", new StringLiteralExpr(discriminator)),
                new MemberValuePair("access", new FieldAccessExpr(new NameExpr("JsonProperty.Access"), "READ_ONLY"))
              )
            )
          )
        })

        terms.foreach({
          case ParameterTerm(propertyName, parameterName, fieldType, _, _) =>
            val field: FieldDeclaration = dtoClass.addField(fieldType, parameterName, PRIVATE, FINAL)
            field.addSingleMemberAnnotation("JsonProperty", new StringLiteralExpr(propertyName))
        })

        val primaryConstructor = dtoClass.addConstructor(PRIVATE)
        primaryConstructor.addMarkerAnnotation("JsonCreator")
        primaryConstructor.setParameters(
          new NodeList(
            terms.map({
              case ParameterTerm(propertyName, parameterName, fieldType, _, _) =>
                new Parameter(util.EnumSet.of(FINAL), fieldType, new SimpleName(parameterName))
                  .addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(propertyName)))
            }): _*
          )
        )
        primaryConstructor.setBody(
          new BlockStmt(
            new NodeList(
              terms.map({
                case ParameterTerm(_, parameterName, fieldType, _, _) =>
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, parameterName),
                      fieldType match {
                        case _: PrimitiveType => new NameExpr(parameterName)
                        case _                => new MethodCallExpr("requireNonNull", new NameExpr(parameterName))
                      },
                      AssignExpr.Operator.ASSIGN
                    )
                  )
              }): _*
            )
          )
        )

        // TODO: handle emptyToNull in the return for the getters
        terms.foreach({
          case ParameterTerm(_, parameterName, fieldType, _, _) =>
            val method = dtoClass.addMethod(s"get${parameterName.capitalize}", PUBLIC)
            method.setType(fieldType)
            method.setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(new FieldAccessExpr(new ThisExpr, parameterName))
                )
              )
            )
        })

        val toStringFieldExprs = NonEmptyList
          .fromList(terms)
          .toList
          .flatMap(
            l =>
              (new StringLiteralExpr(s"${l.head.parameterName}="), new FieldAccessExpr(new ThisExpr, l.head.parameterName)) +:
                l.tail.map(
                param =>
                  (
                    new StringLiteralExpr(s", ${param.parameterName}="),
                    new FieldAccessExpr(new ThisExpr, param.parameterName)
                )
            )
          )

        val toStringMethod = dtoClass
          .addMethod("toString", PUBLIC)
          .setType(STRING_TYPE)
          .addMarkerAnnotation("Override")
        toStringMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(
                new BinaryExpr(
                  toStringFieldExprs.foldLeft[Expression](new StringLiteralExpr(s"${clsName}{"))({
                    case (prevExpr, (strExpr, fieldExpr)) =>
                      new BinaryExpr(
                        new BinaryExpr(prevExpr, strExpr, BinaryExpr.Operator.PLUS),
                        fieldExpr,
                        BinaryExpr.Operator.PLUS
                      )
                  }),
                  new StringLiteralExpr("}"),
                  BinaryExpr.Operator.PLUS
                )
              )
            )
          )
        )

        val equalsConditions: List[Expression] = terms.map({
          case ParameterTerm(_, parameterName, fieldType, _, _) =>
            fieldType match {
              case _: PrimitiveType =>
                new BinaryExpr(
                  new FieldAccessExpr(new ThisExpr, parameterName),
                  new FieldAccessExpr(new NameExpr("other"), parameterName),
                  BinaryExpr.Operator.EQUALS
                )
              case _ =>
                new MethodCallExpr(
                  new FieldAccessExpr(new ThisExpr, parameterName),
                  "equals",
                  new NodeList[Expression](new FieldAccessExpr(new NameExpr("other"), parameterName))
                )
            }
        })
        val returnExpr = NonEmptyList
          .fromList(equalsConditions)
          .map(
            _.reduceLeft(
              (prevExpr, condExpr) => new BinaryExpr(prevExpr, condExpr, BinaryExpr.Operator.AND)
            )
          )
          .getOrElse(new BooleanLiteralExpr(true))

        val equalsMethod = dtoClass
          .addMethod("equals", PUBLIC)
          .setType(PrimitiveType.booleanType)
          .addMarkerAnnotation("Override")
          .addParameter(new Parameter(util.EnumSet.of(FINAL), OBJECT_TYPE, new SimpleName("o")))
        equalsMethod.setBody(
          new BlockStmt(
            new NodeList(
              new IfStmt(
                new BinaryExpr(new ThisExpr, new NameExpr("o"), BinaryExpr.Operator.EQUALS),
                new BlockStmt(new NodeList(new ReturnStmt(new BooleanLiteralExpr(true)))),
                null
              ),
              new IfStmt(
                new BinaryExpr(
                  new BinaryExpr(new NameExpr("o"), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                  new BinaryExpr(new MethodCallExpr("getClass"), new MethodCallExpr(new NameExpr("o"), "getClass"), BinaryExpr.Operator.NOT_EQUALS),
                  BinaryExpr.Operator.OR
                ),
                new BlockStmt(new NodeList(new ReturnStmt(new BooleanLiteralExpr(false)))),
                null
              ),
              new ExpressionStmt(
                new VariableDeclarationExpr(new VariableDeclarator(
                                              dtoClassType,
                                              "other",
                                              new CastExpr(dtoClassType, new NameExpr("o"))
                                            ),
                                            FINAL)
              ),
              new ReturnStmt(returnExpr)
            )
          )
        )

        val hashCodeMethod = dtoClass
          .addMethod("hashCode", PUBLIC)
          .setType(PrimitiveType.intType)
          .addMarkerAnnotation("Override")
        hashCodeMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(
                new MethodCallExpr(
                  new NameExpr("java.util.Objects"),
                  "hash",
                  new NodeList[Expression](terms.map(term => new FieldAccessExpr(new ThisExpr, term.parameterName)): _*)
                )
              )
            )
          )
        )

        val builderMethod = dtoClass.addMethod("builder", PUBLIC, STATIC)
        builderMethod.setType("Builder")
        builderMethod.setParameters(
          new NodeList(
            requiredTerms.map({
              case ParameterTerm(_, parameterName, _, parameterType, _) =>
                new Parameter(util.EnumSet.of(FINAL), parameterType, new SimpleName(parameterName))
            }): _*
          )
        )
        builderMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(
                new ObjectCreationExpr(
                  null,
                  JavaParser.parseClassOrInterfaceType("Builder"),
                  new NodeList(requiredTerms.map({
                    case ParameterTerm(_, parameterName, _, _, _) => new NameExpr(parameterName)
                  }): _*)
                )
              )
            )
          )
        )

        val builderClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, STATIC), false, "Builder")

        requiredTerms.foreach({
          case ParameterTerm(_, parameterName, fieldType, _, _) =>
            builderClass.addField(fieldType, parameterName, PRIVATE, FINAL)
        })
        optionalTerms.foreach({
          case ParameterTerm(_, parameterName, fieldType, _, defaultValue) =>
            val initializer = defaultValue.fold[Expression](
              new MethodCallExpr(new NameExpr("java.util.Optional"), "empty")
            )(
              dv =>
                if (fieldType.isOptional) {
                  new MethodCallExpr(new NameExpr("java.util.Optional"), "of", new NodeList[Expression](dv))
                } else {
                  dv
              }
            )
            builderClass.addFieldWithInitializer(fieldType, parameterName, initializer, PRIVATE)
        })

        val builderConstructor = builderClass.addConstructor(PRIVATE)
        builderConstructor.setParameters(
          new NodeList(
            requiredTerms.map({
              case ParameterTerm(_, parameterName, _, parameterType, _) =>
                new Parameter(util.EnumSet.of(FINAL), parameterType, new SimpleName(parameterName))
            }): _*
          )
        )
        builderConstructor.setBody(
          new BlockStmt(
            new NodeList(
              requiredTerms.map({
                case ParameterTerm(_, parameterName, fieldType, _, _) =>
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, parameterName),
                      fieldType match {
                        case _: PrimitiveType => new NameExpr(parameterName)
                        case _                => new MethodCallExpr("requireNonNull", new NameExpr(parameterName))
                      },
                      AssignExpr.Operator.ASSIGN
                    )
                  )
              }): _*
            )
          )
        )

        // TODO: leave out with${name}() if readOnlyKey?
        optionalTerms.foreach({
          case ParameterTerm(_, parameterName, fieldType, parameterType, _) =>
            val setter = builderClass.addMethod(s"with${parameterName.capitalize}", PUBLIC)
            setter.setType("Builder")
            setter.addAndGetParameter(new Parameter(util.EnumSet.of(FINAL), parameterType, new SimpleName(parameterName)))
            setter.setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, parameterName),
                      if (fieldType.isOptional) new MethodCallExpr("java.util.Optional.of", new NameExpr(parameterName)) else new NameExpr(parameterName),
                      AssignExpr.Operator.ASSIGN
                    )
                  ),
                  new ReturnStmt(new ThisExpr)
                )
              )
            )
        })

        val buildMethod = builderClass.addMethod("build", PUBLIC)
        buildMethod.setType(clsName)
        buildMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(
                new ObjectCreationExpr(
                  null,
                  JavaParser.parseClassOrInterfaceType(clsName),
                  new NodeList(terms.map(param => new FieldAccessExpr(new ThisExpr, param.parameterName)): _*)
                )
              )
            )
          )
        )

        dtoClass.addMember(builderClass)

        Target.pure(dtoClass)

      case EncodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        Target.pure(None)

      case DecodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        Target.pure(None)

      case RenderDTOStaticDefns(clsName, deps, encoder, decoder) =>
        Target.pure(StaticDefns(clsName, List.empty, List.empty))
    }
  }

  object ArrayProtocolTermInterp extends (ArrayProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ArrayProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractArrayType(arr, concreteTypes) =>
        for {
          result <- arr match {
            case SwaggerUtil.Resolved(tpe, dep, default) => Target.pure(tpe)
            case SwaggerUtil.Deferred(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(Target.pure(_)), s"Unresolved reference ${tpeName}").flatten
            case SwaggerUtil.DeferredArray(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => safeParseType(s"Array<${tpe}>")), s"Unresolved reference ${tpeName}").flatten
            case SwaggerUtil.DeferredMap(tpeName) =>
              Target
                .fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => safeParseType(s"Array<Map<String, ${tpe}>>")), s"Unresolved reference ${tpeName}")
                .flatten
          }
        } yield result
    }
  }

  object ProtocolSupportTermInterp extends (ProtocolSupportTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ProtocolSupportTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractConcreteTypes(definitions) =>
        definitions.fold[Target[List[PropMeta[JavaLanguage]]]](Target.raiseError, Target.pure)

      case ProtocolImports() =>
        (List(
          "com.fasterxml.jackson.annotation.JsonCreator",
          "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
          "com.fasterxml.jackson.annotation.JsonProperty"
        ).map(safeParseRawImport) ++ List(
          "java.util.Objects.requireNonNull"
        ).map(safeParseRawStaticImport)).sequence

      case PackageObjectImports() =>
        Target.pure(List.empty)

      case PackageObjectContents() =>
        Target.pure(List.empty)
    }
  }

  object PolyProtocolTermInterp extends (PolyProtocolTerm[JavaLanguage, ?] ~> Target) {
    override def apply[A](fa: PolyProtocolTerm[JavaLanguage, A]): Target[A] = fa match {
      case ExtractSuperClass(swagger, definitions) =>
        def allParents(model: Schema[_]): List[(String, Schema[_], List[Schema[_]])] =
          model match {
            case elem: ComposedSchema =>
              Option(elem.getAllOf).map(_.asScala.toList).getOrElse(List.empty) match {
                case head :: tail =>
                  definitions
                    .collectFirst({
                      case (clsName, e) if Option(head.get$ref).exists(_.endsWith(s"/$clsName")) =>
                        (clsName, e, tail.toList) :: allParents(e)
                    })
                    .getOrElse(List.empty)
                case _ => List.empty
              }
            case _ => List.empty
          }

        Target.pure(allParents(swagger))

      case RenderADTStaticDefns(clsName, discriminator, encoder, decoder) =>
        for {
          extraImports <- List(
            "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
            "com.fasterxml.jackson.annotation.JsonSubTypes",
            "com.fasterxml.jackson.annotation.JsonTypeInfo"
          ).traverse(safeParseRawImport)
        } yield
          StaticDefns[JavaLanguage](
            clsName,
            extraImports,
            List.empty
          )

      case DecodeADT(clsName, children) =>
        Target.pure(None)

      case EncodeADT(clsName, children) =>
        Target.pure(None)

      case RenderSealedTrait(className, selfParams, discriminator, parents, children) =>
        val parentOpt                      = parents.headOption
        val params                         = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(_.term.getName.getId == discriminator)
        val (requiredTerms, optionalTerms) = sortParams(params)
        val terms                          = requiredTerms ++ optionalTerms

        val abstractClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, ABSTRACT), false, className)
        abstractClass.addAnnotation(
          new NormalAnnotationExpr(
            new Name("JsonIgnoreProperties"),
            new NodeList(
              new MemberValuePair(
                "ignoreUnknown",
                new BooleanLiteralExpr(true)
              )
            )
          )
        )
        abstractClass.addAnnotation(
          new NormalAnnotationExpr(
            new Name("JsonTypeInfo"),
            new NodeList(
              new MemberValuePair(
                "use",
                new FieldAccessExpr(new NameExpr("JsonTypeInfo.Id"), "NAME")
              ),
              new MemberValuePair(
                "include",
                new FieldAccessExpr(new NameExpr("JsonTypeInfo.As"), "PROPERTY")
              ),
              new MemberValuePair(
                "property",
                new StringLiteralExpr(discriminator)
              )
            )
          )
        )
        abstractClass.addSingleMemberAnnotation(
          "JsonSubTypes",
          new ArrayInitializerExpr(
            new NodeList(
              children.map(
                child =>
                  new NormalAnnotationExpr(
                    new Name("JsonSubTypes.Type"),
                    new NodeList(
                      new MemberValuePair("name", new StringLiteralExpr(child)),
                      new MemberValuePair("value", new ClassExpr(JavaParser.parseType(child)))
                    )
                )
              ): _*
            )
          )
        )

        addParents(abstractClass, parentOpt)

        terms.foreach({
          case ParameterTerm(_, parameterName, fieldType, _, _) =>
            val method: MethodDeclaration = abstractClass.addMethod(s"get${parameterName.capitalize}", PUBLIC, ABSTRACT)
            method.setType(fieldType)
            method.setBody(null)
        })

        Target.pure(abstractClass)
    }
  }
}
