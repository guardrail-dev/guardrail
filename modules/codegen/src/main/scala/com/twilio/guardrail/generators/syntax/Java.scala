package com.twilio.guardrail.generators.syntax

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.Parameter
import com.github.javaparser.ast.expr.{ Expression, Name, SimpleName }
import com.github.javaparser.printer.PrettyPrinterConfiguration
import com.github.javaparser.printer.PrettyPrinterConfiguration.IndentType
import com.twilio.guardrail.Target
import scala.reflect.ClassTag
import scala.util.Try

object Java {
  private[this] def safeParse[T](parser: String => T, s: String)(implicit cls: ClassTag[T]): Target[T] =
    Try(parser(s)).toEither.fold(t => Target.raiseError(s"Unable to parse '${t}' to a ${cls.getClass.getSimpleName}: ${t.getMessage}"), Target.pure)

  def safeParseCode(s: String): Target[CompilationUnit]                                     = safeParse(JavaParser.parse, s)
  def safeParseSimpleName(s: String): Target[SimpleName]                                    = safeParse(JavaParser.parseSimpleName, s)
  def safeParseName(s: String): Target[Name]                                                = safeParse(JavaParser.parseName, s)
  def safeParseType(s: String): Target[Type]                                                = safeParse(JavaParser.parseType, s)
  def safeParseExpression[T <: Expression](s: String)(implicit cls: ClassTag[T]): Target[T] = safeParse[T](JavaParser.parseExpression, s)
  def safeParseParameter(s: String): Target[Parameter]                                      = safeParse(JavaParser.parseParameter, s)

  val printer: PrettyPrinterConfiguration = new PrettyPrinterConfiguration()
    .setColumnAlignFirstMethodChain(true)
    .setColumnAlignParameters(true)
    .setIndentSize(4)
    .setIndentType(IndentType.SPACES)
    .setOrderImports(true)
    .setPrintComments(true)
    .setPrintJavadoc(true)
    .setTabWidth(4)
}
