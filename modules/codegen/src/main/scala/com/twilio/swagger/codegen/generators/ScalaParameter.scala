package com.twilio.swagger.codegen
package generators

import _root_.io.swagger.models.parameters.Parameter
import java.util.Locale
import scala.meta._
import cats.syntax.traverse._
import cats.instances.all._

case class ScalaParameter(in: Option[String], param: Term.Param, paramName: Term.Name, argName: Term.Name, argType: Type)
object ScalaParameter {
  def fromParam: Term.Param => ScalaParameter = { param => fromParam(param.name.value)(param) }
  def fromParam(argName: String): Term.Param => ScalaParameter = fromParam(Term.Name(argName))
  def fromParam(argName: Term.Name): Term.Param => ScalaParameter = { case param@Term.Param(mods, name, decltype, default) =>
    val tpe: Type = decltype.flatMap({
      case Type.ByName(tpe) => Some(tpe)
      case _ => None
    }).getOrElse(t"Nothing")
    ScalaParameter(None, param, Term.Name(name.value), argName, tpe)
  }

  def fromParameter(protocolElems: List[StrictProtocolElems]): Parameter => Target[ScalaParameter] = { parameter =>
    def toCamelCase(s: String): String = {
      val fromSnakeOrDashed = "[_-]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))
      "^([A-Z])".r.replaceAllIn(fromSnakeOrDashed, m => m.group(1).toLowerCase(Locale.US))
    }

    def paramMeta[T <: Parameter](param: T): Target[SwaggerUtil.ResolvedType] = {
      import com.twilio.swagger.codegen.extract.{Default, ScalaType}
      import _root_.io.swagger.models.parameters._
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
          rtpe <- SwaggerUtil.modelMetaType(schema)
        } yield rtpe
        case x: HeaderParameter =>
          for {
            tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
          } yield SwaggerUtil.Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
        case x: PathParameter =>
          for {
            tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
          } yield SwaggerUtil.Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
        case x: QueryParameter =>
          for {
            tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
          } yield SwaggerUtil.Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
        case x: CookieParameter =>
          for {
            tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
          } yield SwaggerUtil.Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
        case x: FormParameter =>
          for {
            tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
          } yield SwaggerUtil.Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
        case r: RefParameter =>
          for {
            tpeName <- Target.fromOption(Option(r.getSimpleRef()), "$ref not defined")
          } yield SwaggerUtil.Deferred(tpeName)
        case x: SerializableParameter =>
          for {
            tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
          } yield SwaggerUtil.Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, None)
        case x =>
          Target.error(s"Unsure how to handle ${x}")
      }
    }

    for {
      meta <- paramMeta(parameter)
      resolved <- SwaggerUtil.ResolvedType.resolve(meta, protocolElems)
      SwaggerUtil.Resolved(baseType, _, baseDefaultValue) = resolved
      paramType = baseType match {
        case t"java.io.File" if Option(parameter.getIn) == Some("formData") => t"BodyPartEntity"
        case other => other
      }

      required = parameter.getRequired()
      declType: Type = if (!required) {
        t"Option[$paramType]"
      } else {
        paramType
      }

      enumDefaultValue <- (paramType match {
        case tpe@Type.Name(tpeName) =>
          protocolElems.collect({ case x@EnumDefinition(_, Type.Name(`tpeName`), _, _, _) => x }).headOption
            .fold(baseDefaultValue.map(Target.pure _)) { case EnumDefinition(_, _, elems, _, _) => // FIXME: Is there a better way to do this? There's a gap of coverage here
              baseDefaultValue.map {
                case Lit.String(name) =>
                  elems.find(_._1 == name)
                    .fold(Target.error[Term](s"Enumeration ${tpeName} is not defined for default value ${name}"))(value => Target.pure(value._3))
                case _ => Target.error[Term](s"Enumeration ${tpeName} somehow has a default value that isn't a string")
              }
            }
        case _ => baseDefaultValue.map(Target.pure _)
      }).sequenceU
    } yield {
      val defaultValue = if (!required) {
        enumDefaultValue.map(x => q"Option(${x})").orElse(Some(q"None"))
      } else {
        enumDefaultValue
      }

      val paramName = Term.Name(toCamelCase(parameter.getName))
      val param = param"${paramName}: ${declType}".copy(default=defaultValue)
      ScalaParameter(Option(parameter.getIn), param, paramName, Term.Name(parameter.getName), declType)
    }
  }

  def fromParameters(protocolElems: List[StrictProtocolElems]): List[Parameter] => Target[List[ScalaParameter]] = { params =>
    for {
      parameters <- params.map(fromParameter(protocolElems)).sequenceU
      counts = parameters.groupBy(_.paramName.value).mapValues(_.length)
    } yield parameters.map { param =>
        val Term.Name(name) = param.paramName
        if (counts.getOrElse(name, 0) > 1) {
          val escapedName = Term.Name(SwaggerUtil.escapeReserved(param.argName.value))
          param.copy(
            param = param.param.copy(name=escapedName),
            paramName = escapedName
          )
        }
        else param
      }
  }

  /**
    * Create method parameters from Swagger's Path parameters list. Use Option for non-required parameters.
    * @param params
    * @return
    */
  def filterParams(params: List[ScalaParameter]): String => List[ScalaParameter] = { in =>
    params.filter(_.in == Some(in))
  }
}
