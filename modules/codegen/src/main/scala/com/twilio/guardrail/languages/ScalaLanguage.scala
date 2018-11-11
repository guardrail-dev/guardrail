package com.twilio.guardrail.languages

class ScalaLanguage extends LanguageAbstraction {

  type Statement = scala.meta.Stat

  type Import = scala.meta.Import

  // Terms

  type Term     = scala.meta.Term
  type TermName = scala.meta.Term.Name

  // Declarations
  type MethodDeclaration = scala.meta.Decl.Def

  // Definitions
  type Definition          = scala.meta.Defn
  type AbstractClass       = Nothing
  type ClassDefinition     = scala.meta.Defn.Class
  type InterfaceDefinition = Nothing
  type ObjectDefinition    = scala.meta.Defn.Object
  type Trait               = scala.meta.Defn.Trait

  // Functions
  type InstanceMethod = Nothing
  type StaticMethod   = Nothing

  // Values
  type ValueDefinition = scala.meta.Defn.Val
  type MethodParameter = scala.meta.Term.Param
  type Type            = scala.meta.Type
  type TypeName        = scala.meta.Type.Name

  // Result
  type FileContents = scala.meta.Source
}
