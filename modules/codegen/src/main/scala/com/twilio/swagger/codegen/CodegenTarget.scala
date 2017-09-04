package com.twilio.swagger.codegen

sealed trait CodegenTarget
object CodegenTarget {
  case object Client extends CodegenTarget
  case object Server extends CodegenTarget
}
