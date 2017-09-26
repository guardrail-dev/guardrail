package com.twilio.swagger.codegen

import cats.syntax.either._
import scala.collection.immutable.Seq

case class Args(
  kind: CodegenTarget
, specPath: Option[String]
, outputPath: Option[String]
, packageName: Option[Seq[String]]
, dtoPackage: Seq[String]
, printHelp: Boolean
, context: Context
, defaults: Boolean
)

object Args {
  val empty = Args(CodegenTarget.Client, Option.empty, Option.empty, Option.empty, Seq.empty, false, Context.empty, false)
  def isEmpty: Args => Boolean = { args =>
    args.specPath.isEmpty &&
    args.outputPath.isEmpty &&
    args.packageName.isEmpty
  }
}
