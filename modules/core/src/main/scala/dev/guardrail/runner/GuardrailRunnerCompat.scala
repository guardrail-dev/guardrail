package dev.guardrail.runner

import cats.data.NonEmptyList

import dev.guardrail.{ Args, ReadSwagger, Target, WriteTree }

abstract class GuardrailRunnerCompat(val languages: Map[String, NonEmptyList[Args] => Target[NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]]])
    extends GuardrailRunner
