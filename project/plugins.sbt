addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.3")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")

addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.7")

addSbtPlugin("ohnosequences" % "sbt-github-release" % "0.7.0")

addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.13")

addSbtPlugin("com.47deg"  % "sbt-microsites" % "1.3.4")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.27")

addSbtPlugin("org.scoverage"    %% "sbt-scoverage"  % "1.7.3")

// for jdk11
libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0"
