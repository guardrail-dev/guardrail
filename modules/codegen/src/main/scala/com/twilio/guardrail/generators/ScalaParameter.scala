package com.twilio.guardrail
package generators

import _root_.io.swagger.models.parameters.Parameter
import com.twilio.guardrail.extract.{ Default, ScalaFileHashAlgorithm, ScalaType }
import java.util.Locale

import scala.meta._
import cats.syntax.traverse._
import cats.instances.all._

class GeneratorSettings(val fileType: Type, val jsonType: Type)
case class RawParameterName private[generators] (value: String) {
  def toLit: Lit.String = Lit.String(value)
}
class ScalaParameter private[generators] (
    val in: Option[String],
    val param: Term.Param,
    val paramName: Term.Name,
    val argName: RawParameterName,
    val argType: Type,
    val required: Boolean,
    val hashAlgorithm: Option[String],
    val isFile: Boolean
) {
  override def toString: String =
    s"ScalaParameter(${in}, ${param}, ${paramName}, ${argName}, ${argType})"

  def withType(newArgType: Type): ScalaParameter =
    new ScalaParameter(in, param, paramName, argName, newArgType, required, hashAlgorithm, isFile)
}
object ScalaParameter {
  def unapply(param: ScalaParameter): Option[(Option[String], Term.Param, Term.Name, RawParameterName, Type)] =
    Some((param.in, param.param, param.paramName, param.argName, param.argType))

  def fromParam(param: Term.Param)(implicit gs: GeneratorSettings): ScalaParameter =
    fromParam(param.name.value)(param)
  def fromParam(argName: String)(param: Term.Param)(implicit gs: GeneratorSettings): ScalaParameter =
    fromParam(RawParameterName(argName))(param)
  def fromParam(argName: RawParameterName)(param: Term.Param)(implicit gs: GeneratorSettings): ScalaParameter = param match {
    case param @ Term.Param(mods, name, decltype, default) =>
      val (tpe, innerTpe, required): (Type, Type, Boolean) = decltype
        .flatMap({
          case tpe @ t"Option[$inner]" =>
            Some((tpe, inner, false))
          case Type.ByName(tpe)   => Some((tpe, tpe, true))
          case tpe @ Type.Name(_) => Some((tpe, tpe, true))
          case _                  => None
        })
        .getOrElse((t"Nothing", t"Nothing", true))
      new ScalaParameter(None, param, Term.Name(name.value), argName, tpe, required, None, innerTpe == gs.fileType)
  }

  def fromParameter(protocolElems: List[StrictProtocolElems]): Parameter => Target[ScalaParameter] = { parameter =>
    def toCamelCase(s: String): String = {
      val fromSnakeOrDashed =
        "[_-]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))
      "^([A-Z])".r
        .replaceAllIn(fromSnakeOrDashed, m => m.group(1).toLowerCase(Locale.US))
    }

    def paramMeta[T <: Parameter](param: T): Target[SwaggerUtil.ResolvedType] = {
      import _root_.io.swagger.models.parameters._
      def getDefault[U <: AbstractSerializableParameter[U]: Default.GetDefault](p: U): Option[Term] = (
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
              rtpe   <- SwaggerUtil.modelMetaType(schema)
            } yield rtpe
          case x: HeaderParameter =>
            for {
              tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
            } yield
              SwaggerUtil
                .Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
          case x: PathParameter =>
            for {
              tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
            } yield
              SwaggerUtil
                .Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
          case x: QueryParameter =>
            for {
              tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
            } yield
              SwaggerUtil
                .Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
          case x: CookieParameter =>
            for {
              tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
            } yield
              SwaggerUtil
                .Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
          case x: FormParameter =>
            for {
              tpeName <- Target.fromOption(Option(x.getType()), s"Missing type")
            } yield
              SwaggerUtil
                .Resolved(SwaggerUtil.typeName(tpeName, Option(x.getFormat()), ScalaType(x)), None, getDefault(x))
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
    }

    Target.getGeneratorSettings.flatMap { implicit gs =>
      for {
        meta     <- paramMeta(parameter)
        resolved <- SwaggerUtil.ResolvedType.resolve(meta, protocolElems)
        SwaggerUtil.Resolved(paramType, _, baseDefaultValue) = resolved

        required = parameter.getRequired()
        declType: Type = if (!required) {
          t"Option[$paramType]"
        } else {
          paramType
        }

        enumDefaultValue <- (paramType match {
          case tpe @ Type.Name(tpeName) =>
            protocolElems
              .collect({
                case x @ EnumDefinition(_, Type.Name(`tpeName`), _, _, _) => x
              })
              .headOption
              .fold(baseDefaultValue.map(Target.pure _)) {
                case EnumDefinition(_, _, elems, _, _) => // FIXME: Is there a better way to do this? There's a gap of coverage here
                  baseDefaultValue.map {
                    case Lit.String(name) =>
                      elems
                        .find(_._1 == name)
                        .fold(Target.error[Term](s"Enumeration ${tpeName} is not defined for default value ${name}"))(value => Target.pure(value._3))
                    case _ =>
                      Target.error[Term](s"Enumeration ${tpeName} somehow has a default value that isn't a string")
                  }
              }
          case _ => baseDefaultValue.map(Target.pure _)
        }).sequence

        defaultValue = if (!required) {
          enumDefaultValue.map(x => q"Option(${x})").orElse(Some(q"None"))
        } else {
          enumDefaultValue
        }
        name <- Target.fromOption(Option(parameter.getName), "Parameter missing \"name\"")
      } yield {
        val paramName = Term.Name(toCamelCase(name))
        val param     = param"${paramName}: ${declType}".copy(default = defaultValue)
        new ScalaParameter(Option(parameter.getIn),
                           param,
                           paramName,
                           RawParameterName(name),
                           declType,
                           required,
                           ScalaFileHashAlgorithm(parameter),
                           paramType == gs.fileType)
      }
    }
  }

  def fromParameters(protocolElems: List[StrictProtocolElems]): List[Parameter] => Target[List[ScalaParameter]] = { params =>
    for {
      parameters <- params.traverse(fromParameter(protocolElems))
      counts = parameters.groupBy(_.paramName.value).mapValues(_.length)
    } yield
      parameters.map { param =>
        val Term.Name(name) = param.paramName
        if (counts.getOrElse(name, 0) > 1) {
          val escapedName =
            Term.Name(param.argName.value)
          new ScalaParameter(
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
  def filterParams(params: List[ScalaParameter]): String => List[ScalaParameter] = { in =>
    params.filter(_.in == Some(in))
  }
}
