package dev.guardrail.sbt.interop

import _root_.sbt._
import _root_.sbt.Keys._
import _root_.sbt.util.CacheStore
import _root_.sbt.util.Cache
import _root_.sbt.util.CacheImplicits._

object InteropTasks {
  def buildPOJO(fqcn: scala.meta.Type.Select, parameters: List[scala.meta.Term.Param], extraStaticStats: List[scala.meta.Stat], extraDynamicStats: List[scala.meta.Stat]): scala.meta.Source = {
    import _root_.scala.meta._

    val Type.Select(pkg, typeName) = fqcn

    val Type.Name(clsName) = typeName
    val termName = Term.Name(clsName)

    val accessorParams = parameters.map { case param"$term: $tpe" =>
      param"val $term: $tpe"
    }

    source"""
      package ${pkg}
      object ${termName} {
        ${CompanionFunctions.buildApply(typeName, parameters)};
        ..${extraStaticStats}
      }

      class ${typeName}(..${accessorParams}) { self =>
        ${PojoFunctions.buildToString(clsName, parameters)};
        ..${PojoFunctions.buildWithMethods(typeName, termName, parameters)};
        ..${extraDynamicStats}
      }
    """
  }

  // A utility function to write scalameta ASTs into files
  def writeFiles(outputs: List[(scala.meta.Source, java.io.File)]): List[java.io.File] = {
    import java.nio.file.Files
    import java.nio.file.Path
    import java.io.FileOutputStream
    outputs.map { case (src, dest) =>
      Files.createDirectories(Path.of(dest.getParent()))
      val fos = new FileOutputStream(dest)
      fos.write(src.syntax.getBytes())
      fos.close()
      dest
    }
  }
}
