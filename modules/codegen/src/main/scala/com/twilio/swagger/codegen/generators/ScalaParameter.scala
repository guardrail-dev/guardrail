package com.twilio.swagger.codegen
package generators

import _root_.io.swagger.models.parameters.Parameter
import java.util.Locale
import scala.meta._

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

  def fromParameter(protocolElems: List[ProtocolElems]): Parameter => ScalaParameter = { parameter =>
    def toCamelCase(s: String): String = {
      val fromSnakeOrDashed = "[_-]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))
      "^([A-Z])".r.replaceAllIn(fromSnakeOrDashed, m => m.group(1).toLowerCase(Locale.US))
    }

    val SwaggerUtil.ParamMeta(baseType, baseDefaultValue) = Target.unsafeExtract(SwaggerUtil.paramMeta(parameter))
    val paramType = baseType match {
      case t"java.io.File" if Option(parameter.getIn) == Some("formData") => t"BodyPartEntity"
      case other => other
    }

    val required = parameter.getRequired()
    val declType: Type = if (!required) {
      t"Option[$paramType]"
    } else {
      paramType
    }

    val enumType: Option[Type.Name] = paramType match {
      case tpe@Type.Name(_) => Some(tpe)
      case _ => None
    }

    val propDefaultValue: Option[Term] =
      enumType.flatMap { case Type.Name(tpeName) =>
        protocolElems
          .flatMap {
            case EnumDefinition(Type.Name(`tpeName`), elems, _, _) =>
              baseDefaultValue.flatMap {
                case Lit.String(name) => elems.find(_._1 == name).map(_._3) // FIXME: Failed lookups don't fail codegen, causing mismatches like `foo: Bar = "baz"`
              }
            case _ => None
          } headOption
      } orElse baseDefaultValue

    val defaultValue = if (!required) {
      propDefaultValue.map(x => q"Option(${x})").orElse(Some(q"None"))
    } else {
      propDefaultValue
    }

    val paramName = Term.Name(toCamelCase(parameter.getName))
    val param = param"${paramName}: ${declType}".copy(default=defaultValue)
    ScalaParameter(Option(parameter.getIn), param, paramName, Term.Name(parameter.getName), declType)
  }

  def fromParameters(protocolElems: List[ProtocolElems]): List[Parameter] => List[ScalaParameter] = { params =>
    val parameters = params.map(ScalaParameter.fromParameter(protocolElems))
    val counts = parameters.groupBy(_.paramName.value).mapValues(_.length)

    parameters.map { param =>
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
