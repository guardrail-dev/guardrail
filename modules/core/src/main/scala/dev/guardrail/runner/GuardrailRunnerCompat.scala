package dev.guardrail.runner

import cats.data.NonEmptyList

import dev.guardrail.{ Args, ReadSpec, Target, WriteTree }

abstract class GuardrailRunnerCompat(val languages: Map[String, NonEmptyList[Args] => Target[NonEmptyList[ReadSpec[Target[List[WriteTree]]]]]])
    extends GuardrailRunner
