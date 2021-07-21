package dev.guardrail.languages

class LanguageAbstraction {

  type Statement

  type Import

  // Terms

  type Term
  type TermName
  type TermSelect
  type Expression
  type Apply

  // Declarations
  type MethodDeclaration

  // Definitions
  type Definition
  type AbstractClass
  type ClassDefinition
  type InterfaceDefinition
  type ObjectDefinition
  type Trait

  // Functions
  type InstanceMethod
  type StaticMethod

  // Values
  type ValueDefinition
  type MethodParameter
  type Type
  type TypeName
  type Annotation

  // Result
  type FileContents
}
