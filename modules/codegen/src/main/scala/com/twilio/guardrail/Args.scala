package com.twilio.guardrail

case class Args(
    kind: CodegenTarget,
    specPath: Option[String],
    outputPath: Option[String],
    packageName: Option[List[String]],
    dtoPackage: List[String],
    printHelp: Boolean,
    context: Context,
    defaults: Boolean,
    imports: List[String]
)

object Args {
  val empty: Args = Args(CodegenTarget.Client, Option.empty, Option.empty, Option.empty, List.empty, false, Context.empty, false, List.empty)
  def isEmpty: Args => Boolean = { args =>
    args.specPath.isEmpty &&
    args.outputPath.isEmpty &&
    args.packageName.isEmpty
  }
}
