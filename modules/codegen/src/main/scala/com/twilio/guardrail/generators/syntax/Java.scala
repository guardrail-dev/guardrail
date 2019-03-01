package com.twilio.guardrail.generators.syntax

import cats.implicits._
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration}
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.Parameter
import com.github.javaparser.ast.expr.{Expression, Name, SimpleName}
import com.github.javaparser.printer.PrettyPrinterConfiguration
import com.github.javaparser.printer.PrettyPrinterConfiguration.IndentType
import com.twilio.guardrail.Target
import java.util.Optional
import scala.reflect.ClassTag
import scala.util.Try

object Java {
  implicit class RichJavaOptional[T](val o: Optional[T]) extends AnyVal {
    def asScala: Option[T] = if (o.isPresent) Option(o.get) else None
  }

  private[this] def safeParse[T](log: String)(parser: String => T, s: String)(implicit cls: ClassTag[T]): Target[T] = {
    Target.log.debug(log)(s) >> (
      Try(parser(s)).toEither.fold(t => Target.raiseError(s"Unable to parse '${s}' to a ${cls.runtimeClass.getName}: ${t.getMessage}"), Target.pure)
    )
  }

  def safeParseCode(s: String): Target[CompilationUnit]                                     = safeParse("safeParseCode")(JavaParser.parse, s)
  def safeParseSimpleName(s: String): Target[SimpleName]                                    = safeParse("safeParseSimpleName")(JavaParser.parseSimpleName, s)
  def safeParseName(s: String): Target[Name]                                                = safeParse("safeParseName")(JavaParser.parseName, s)
  def safeParseType(s: String): Target[Type]                                                = safeParse("safeParseType")(JavaParser.parseType, s)
  def safeParseExpression[T <: Expression](s: String)(implicit cls: ClassTag[T]): Target[T] = safeParse[T]("safeParseExpression")(JavaParser.parseExpression[T], s)
  def safeParseParameter(s: String): Target[Parameter]                                      = safeParse("safeParseParameter")(JavaParser.parseParameter, s)
  def safeParseImport(s: String): Target[ImportDeclaration]                                 = safeParse("safeParseImport")(JavaParser.parseImport, s)
  def safeParseRawImport(s: String): Target[ImportDeclaration]                              = safeParse("safeParseRawImport")(JavaParser.parseImport, s"import ${s};")
  def safeParseRawStaticImport(s: String): Target[ImportDeclaration]                        = safeParse("safeParseStaticImport")(JavaParser.parseImport, s"import static ${s};")

  val printer: PrettyPrinterConfiguration = new PrettyPrinterConfiguration()
    .setColumnAlignFirstMethodChain(true)
    .setColumnAlignParameters(true)
    .setIndentSize(4)
    .setIndentType(IndentType.SPACES)
    .setOrderImports(true)
    .setPrintComments(true)
    .setPrintJavadoc(true)
    .setTabWidth(4)

/*
  implicit class PrintStructure(value: Node) {
    def toAST: String = {
      @scala.annotation.tailrec
      def walk(chunks: List[(String, Int, List[Node], String)]) = {
        chunks.flatMap { case (pre, level, nodes, post) =>
          
        }
      }

      walk(List(("", 0, List(value), "")))
    }
  }
*/
}
