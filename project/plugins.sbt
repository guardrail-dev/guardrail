addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.10")

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.6")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.4")

addSbtPlugin("io.crashbox" % "sbt-gpg" % "0.2.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.4")

addSbtPlugin("ohnosequences" % "sbt-github-release" % "0.7.0")

addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.10")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.33")

addSbtPlugin("com.47deg"  % "sbt-microsites" % "1.2.1")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.19")

addSbtPlugin("org.scoverage"    %% "sbt-scoverage"  % "1.6.1")

// for jdk11
libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0"
