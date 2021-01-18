addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")

addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.5")

addSbtPlugin("ohnosequences" % "sbt-github-release" % "0.7.0")

addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.13")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.33")

addSbtPlugin("com.47deg"  % "sbt-microsites" % "1.2.1")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.25")

addSbtPlugin("org.scoverage"    %% "sbt-scoverage"  % "1.6.1")

// for jdk11
libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0"
