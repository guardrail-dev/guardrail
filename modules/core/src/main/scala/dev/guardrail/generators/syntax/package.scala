package dev.guardrail.generators

import cats.data.NonEmptyList
import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.oas.models.parameters.Parameter
import io.swagger.v3.oas.models.{ Operation, PathItem }
import java.util.Locale
import java.util.regex.Matcher.quoteReplacement

package syntax {
  class RichNotNullShower[A](value: A) {
    def showNotNull: String = showNotNullIndented(0)
    def showNotNullIndented(indent: Int): String =
      ("  " * indent) + value.toString().linesIterator.filterNot(_.contains(": null")).mkString("\n" + ("  " * indent))
  }
}

package object syntax extends SpecializedSyntax {
  val GENERATED_CODE_COMMENT_LINES: List[String] = List(
    "This file was generated by guardrail (https://github.com/guardrail-dev/guardrail).",
    "Modifications will be overwritten; instead edit the OpenAPI/Swagger spec file."
  )

  /*
   * The case converters work as follows.
   *
   * First we break up the given string into parts.  This is a several-stage process
   * while we consider boundaries in precedence order.  First we split on
   * dash/underscore/space/dot, as those are the "strongest" boundary delimiters.
   * Then we split on non-uppercase -> uppercase boundaries.  We avoid splitting
   * just on uppercase because if a part is ALLUPPERCASE then we want to consider it
   * a single part.  After that, we try to keep runs of uppercase characters (when
   * they are followed by non-uppercase characters) in the same group; that is,
   * something like "FOOBar" should get broken up into ("foo", "bar").
   *
   * There are a few things we just can't accurately handle, like digits.  If you
   * consider the source string "foo9Bar", there's no way to know if that should
   * be grouped as ("foo9", "bar") or ("foo", "9", "bar").  And for "foo9bar", it's
   * worse: we don't know if it should be  ("foo9bar"), ("foo", "9", "bar"),
   * ("foo9", "bar"), or ("foo", "9bar").  In these cases we'll choose to assume
   * that digits are not initial group characters.
   */
  private val SPLIT_DELIMITERS = "[-_\\s\\.]+".r
  private val BOUNDARY_SPLITTERS = List(
    "([^A-Z])([A-Z])".r,
    "([A-Z]+)([A-Z][a-z]+)".r
  )

  implicit class RichString(private val s: String) extends AnyVal {
    private def splitParts(s: String): List[String] =
      BOUNDARY_SPLITTERS
        .foldLeft(SPLIT_DELIMITERS.split(s))(
          (last, splitter) => last.flatMap(part => splitter.replaceAllIn(part, m => quoteReplacement(m.group(1) + "-" + m.group(2))).split("-"))
        )
        .map(_.toLowerCase(Locale.US))
        .toList

    def toPascalCase: String = splitParts(s).map(_.capitalize).mkString

    def toCamelCase: String =
      NonEmptyList
        .fromList(splitParts(s))
        .fold("")(
          parts => parts.head + parts.tail.map(_.capitalize).mkString
        )

    def toSnakeCase: String = splitParts(s).mkString("_")

    def toDashedCase: String = splitParts(s).mkString("-")

    def uncapitalized: String =
      if (s.nonEmpty) {
        val inUnPacked              = s.toCharArray
        val lowercaseFirstCharacter = Character.toLowerCase(inUnPacked(0))
        new String(lowercaseFirstCharacter +: inUnPacked.tail)
      } else s

  }

  implicit def RichSchema: Schema[_] => RichNotNullShower[Schema[_]]    = new RichNotNullShower[Schema[_]](_)
  implicit def RichOperation: Operation => RichNotNullShower[Operation] = new RichNotNullShower[Operation](_)
  implicit def RichPathItem: PathItem => RichNotNullShower[PathItem]    = new RichNotNullShower[PathItem](_)
  implicit def RichParameter: Parameter => RichNotNullShower[Parameter] = new RichNotNullShower[Parameter](_)
}
