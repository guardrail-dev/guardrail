package com.twilio.guardrail.terms.collections

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, Type }
import com.github.javaparser.ast.expr.{ Expression, MethodCallExpr, NameExpr }
import com.twilio.guardrail.languages.JavaLanguage
import scala.compat.java8.OptionConverters._

object JavaCollectionsHelpers {
  private[collections] def isContainerOfType(tpe: Type, containerClsScope: String, containerClsName: String): Boolean = tpe match {
    case cls: ClassOrInterfaceType =>
      val tpeScope = cls.getScope.asScala
      cls.getNameAsString == containerClsName && (tpeScope.isEmpty || tpeScope.map(_.asString).contains(containerClsScope))
    case _ => false
  }

  private[collections] def doMethodCall[A](callee: Expression, methodName: String, argument: Expression): TermHolder[JavaLanguage, MethodCallExpr, A] =
    TermHolder[JavaLanguage, MethodCallExpr, A](
      new MethodCallExpr(
        callee,
        methodName,
        new NodeList[Expression](argument)
      )
    )

  private[collections] def wrapStream(expr: Expression): MethodCallExpr =
    new MethodCallExpr(
      expr,
      "stream"
    )

  private[collections] def doCollect(expr: Expression): MethodCallExpr =
    new MethodCallExpr(
      expr,
      "collect",
      new NodeList[Expression](new MethodCallExpr(new NameExpr("java.util.stream.Collectors"), "toList"))
    )
}
