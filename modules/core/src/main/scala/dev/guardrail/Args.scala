package dev.guardrail

import dev.guardrail.protocol.terms.protocol.PropertyRequirement

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
) { self =>
  def copyContext(
      framework: Option[String] = self.context.framework,
      customExtraction: Boolean = self.context.customExtraction,
      tracing: Boolean = self.context.tracing,
      modules: List[String] = self.context.modules,
      propertyRequirement: PropertyRequirement.Configured = self.context.propertyRequirement
  ): Args =
    self.copy(
      context = self.context.copy(
        framework = framework,
        customExtraction = customExtraction,
        tracing = tracing,
        modules = modules,
        propertyRequirement = propertyRequirement
      )
    )

  def copyPropertyRequirement(
      encoder: PropertyRequirement.OptionalRequirement = self.context.propertyRequirement.encoder,
      decoder: PropertyRequirement.OptionalRequirement = self.context.propertyRequirement.decoder
  ): Args =
    copyContext(
      propertyRequirement = self.context.propertyRequirement.copy(
        encoder = encoder,
        decoder = decoder
      )
    )
}

object Args {
  val empty: Args = Args(CodegenTarget.Client, Option.empty, Option.empty, Option.empty, List.empty, false, Context.empty, false, List.empty)
  def isEmpty: Args => Boolean = { args =>
    args.specPath.isEmpty &&
    args.outputPath.isEmpty &&
    args.packageName.isEmpty
  }
}
