package com.twilio.swagger.codegen
package generators

import _root_.io.swagger.models.parameters.Parameter
import java.util.Locale
import scala.meta._

case class ScalaParameter(param: Term.Param, paramName: Term.Name, argName: Term.Name, argType: Type)
object ScalaParameter {
  def fromParam: Term.Param => ScalaParameter = { param => fromParam(param.name.value)(param) }
  def fromParam(argName: String): Term.Param => ScalaParameter = fromParam(Term.Name(argName))
  def fromParam(argName: Term.Name): Term.Param => ScalaParameter = { case param@Term.Param(mods, name, decltype, default) =>
    val tpe: Type = decltype.flatMap({
      case Type.ByName(tpe) => Some(tpe)
      case _ => None
    }).getOrElse(t"Nothing")
    ScalaParameter(param, Term.Name(name.value), argName, tpe)
  }

  /**
    * Create method parameters from Swagger's Path parameters list. Use Option for non-required parameters.
    * @param params
    * @return
    */
  def filterParams(params: List[Parameter], protocolElems: List[ProtocolElems]): String => List[ScalaParameter] = { in =>
    def toCamelCase(s: String): String = {
      val fromSnakeOrDashed = "[_-]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))
      "^([A-Z])".r.replaceAllIn(fromSnakeOrDashed, m => m.group(1).toLowerCase(Locale.US))
    }

    for {
      parameter <- params
      if Option(parameter.getIn) == Some(in)
    } yield {
      val SwaggerUtil.ParamMeta(baseType, baseDefaultValue) = SwaggerUtil.paramMeta(parameter)
      val paramType = baseType match {
        case t"java.io.File" if in == "formData" => t"BodyPartEntity"
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
      ScalaParameter(param, paramName, Term.Name(parameter.getName), declType)
    }
  }
}
