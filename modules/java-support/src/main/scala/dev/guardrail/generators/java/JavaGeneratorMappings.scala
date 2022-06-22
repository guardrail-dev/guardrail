package dev.guardrail.generators.java

import com.github.javaparser.StaticJavaParser
import _root_.scala.util.{ Failure, Success, Try }

import dev.guardrail.core.CoreTermInterp
import dev.guardrail.generators.spi.{ FrameworkLoader, ModuleMapperLoader }
import dev.guardrail.UnparseableArgument

object JavaGeneratorMappings {
  implicit def javaInterpreter = new CoreTermInterp[JavaLanguage](
    "dropwizard",
    FrameworkLoader.load[JavaLanguage](_),
    frameworkName => ModuleMapperLoader.load[JavaLanguage](frameworkName),
    { str =>
      Try(StaticJavaParser.parseImport(s"import ${str};")) match {
        case Success(value) => Right(value)
        case Failure(t)     => Left(UnparseableArgument("import", t.getMessage))
      }
    }
  )
}
