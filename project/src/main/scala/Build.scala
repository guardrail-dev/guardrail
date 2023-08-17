package dev.guardrail.sbt

import sbt._
import sbt.Keys._
import scoverage.ScoverageKeys
import com.typesafe.sbt.SbtGit.GitKeys.gitReader
import complete.DefaultParsers._
import com.typesafe.sbt.SbtGit._
import wartremover.WartRemover.autoImport._
import scalafix.sbt.ScalafixPlugin.autoImport._
import xerial.sbt.Sonatype.autoImport._
import sbtversionpolicy.SbtVersionPolicyPlugin.autoImport._

object Build {
  val stableVersion: SettingKey[String] = SettingKey("stable-version")

  val useStableVersions: Boolean = {
    // NB: Currently, any time any PR that breaks semver is merged, it breaks
    //     the build until the next release.
    //
    //     A hack here is to just disable semver checking on master, since
    //     we already gate semver both at PR time as well as later on
    //     during release, so it actually serves no useful purpose to fail
    //     master as well.
    val isMasterBranch = sys.env.get("GITHUB_REF").contains("refs/heads/master")
    val isRelease = sys.env.contains("GUARDRAIL_RELEASE_MODULE")
    val isCi = sys.env.contains("GUARDRAIL_CI")
    if (isCi || isRelease) {
      val ignoreBincompat = {
        import scala.sys.process._
        "support/current-pr-labels.sh"
          .lineStream_!
          .exists(Set("major", "minor").contains)
      }

      val useStableVersions = !isMasterBranch && (isRelease || !ignoreBincompat)
      println(s"isMasterBranch=${isMasterBranch}, isRelease=${isRelease}, ignoreBincompat=${ignoreBincompat}: useStableVersions=${useStableVersions}")
      if (useStableVersions) {
        println(s"  Ensuring bincompat via MiMa during ${sys.env.get("GITHUB_REF")}")
      } else {
        println(s"  Skipping bincompat check on ${sys.env.get("GITHUB_REF")}")
      }

      useStableVersions
    } else false
  }

  def buildSampleProject(name: String, extraLibraryDependencies: Seq[sbt.librarymanagement.ModuleID]) =
    Project(s"sample-${name}", file(s"modules/sample-${name}"))
      .settings(commonSettings)
      .settings(codegenSettings)
      .settings(libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.11.0")
      .settings(
        libraryDependencies ++= extraLibraryDependencies,
        Compile / unmanagedSourceDirectories += baseDirectory.value / "target" / "generated",
        publish / skip := true,
      )

  val excludedWarts = Set(Wart.DefaultArguments, Wart.Product, Wart.Serializable, Wart.Any, Wart.StringPlusAny)

  val codegenSettings = Seq(
    ScoverageKeys.coverageExcludedPackages := "<empty>;dev.guardrail.terms.*;dev.guardrail.protocol.terms.*",
    Compile / compile / wartremoverWarnings ++= Warts.unsafe.filterNot(w => excludedWarts.exists(_.clazz == w.clazz)),
  )

  def ifScalaVersion[A](minorPred: Int => Boolean = _ => true)(value: List[A]): Def.Initialize[Seq[A]] = Def.setting {
    scalaVersion.value.split('.') match {
      case Array("2", minor, bugfix) if minorPred(minor.toInt) => value
      case _                                                   => Nil
    }
  }

  def customTagToVersionNumber(moduleSegment: String, isRelease: Boolean): String => Option[String] = { v =>
    val prefix = s"${moduleSegment}-v"
    val stripPrefix: String => String = _.stripPrefix(prefix)
    val stripSuffix: String => String = if (isRelease) {
      _.replaceAll("-[0-9]+-[0-9a-z]+$", "")
    } else identity _
    if (v.startsWith(prefix)) { Some(stripSuffix(stripPrefix(v))) }
    else { None }
  }

  val commonSettings = Seq(
    organization := "dev.guardrail",
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),

    crossScalaVersions := Seq("2.12.17", "2.13.8"),
    scalaVersion := "2.12.17",

    // early-semver was a mistake. We already have early-semver guaratees during CI, but including this in the publishing POM
    // ensures that independent 0.x versions are incompatible, even though we know they are.
    versionScheme := None,

    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-Ydelambdafy:method",
      "-Yrangepos",
      // "-Ywarn-unused-import",  // TODO: Enable this! https://github.com/guardrail-dev/guardrail/pull/282
      "-feature",
      "-unchecked",
      "-deprecation",
      "-encoding",
      "utf8"
    ),
    Test / scalacOptions -= "-Xfatal-warnings",
    Compile / console / scalacOptions -= "-Xfatal-warnings",
    Compile / consoleQuick / scalacOptions -= "-Xfatal-warnings",
    scalacOptions ++= ifScalaVersion(_ <= 11)(List("-Xexperimental")).value,
    scalacOptions ++= ifScalaVersion(_ == 12)(List("-Ypartial-unification")).value,
    Test / parallelExecution := true,
    addCompilerPlugin("org.typelevel" % "kind-projector"  % "0.13.2" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    addCompilerPlugin(scalafixSemanticdb),
    sonatypeCredentialHost := "s01.oss.sonatype.org",
  )

  def commonModule(moduleSegment: String) =
    baseModule(s"guardrail-${moduleSegment}", moduleSegment, file(s"modules/${moduleSegment}"))

  def baseModule(moduleName: String, moduleSegment: String, path: File): Project =
    Project(id=moduleName, base=path)
      .settings(versionWithGit)
      .settings(
        // None of this stuff can be used because of scoping issues. Everything needs to be inlined to avoid just bubbling up to a singleton, since the keys (scopes?) are only valid at the root, not scoped per project.
        // git.gitDescribePatterns := Seq(s"${moduleSegment}-v*"),
        // git.gitDescribedVersion := gitReader.value.withGit(_.describedVersion(gitDescribePatterns.value)).map(v => customTagToVersionNumber(moduleSegment)(v).getOrElse(v)),
        git.useGitDescribe := true,
        version := {
          val isRelease = sys.env.contains("GUARDRAIL_RELEASE_MODULE")
          val overrideVersion =
            git.overrideVersion(git.versionProperty.value)
          val uncommittedSuffix =
            git.makeUncommittedSignifierSuffix(git.gitUncommittedChanges.value, git.uncommittedSignifier.value)
          val releaseVersion =
            git.releaseVersion(git.gitCurrentTags.value, customTagToVersionNumber(moduleSegment, isRelease), uncommittedSuffix)
          val customGitDescribedVersion = gitReader.value.withGit(_.describedVersion(Seq(s"${moduleSegment}-v*"))).map(v => customTagToVersionNumber(moduleSegment, isRelease)(v).getOrElse(v))
          val describedVersion =
            git.flaggedOptional(git.useGitDescribe.value, git.describeVersion(customGitDescribedVersion, uncommittedSuffix))
          val datedVersion = git.formattedDateVersion.value
          val commitVersion = git.formattedShaVersion.value
          //Now we fall through the potential version numbers...
          git.makeVersion(Seq(
             overrideVersion,
             releaseVersion,
             describedVersion,
             commitVersion
          )) getOrElse datedVersion // For when git isn't there at all.
        },
        stableVersion := {
          // Pull the tag(s) matching the tag scheme, defaulting to 0.0.0
          // for newly created modules that have never been released before
          // (depending on unreleased modules is an error, so this is fine)
          gitReader
            .value
            .withGit(_.describedVersion(Seq(s"${moduleSegment}-v*")))
            .fold("0.0.0")(v => customTagToVersionNumber(moduleSegment, true)(v).getOrElse(v))
        }
      )
      .settings(commonSettings)
      .settings(versionPolicyIntention := {
        val isRelease = sys.env.contains("GUARDRAIL_RELEASE_MODULE")
        if (isRelease) Compatibility.BinaryCompatible else Compatibility.None
      })
      .settings(name := moduleName)
      .settings(codegenSettings)
      .settings(libraryDependencies ++= Dependencies.testDependencies)
      .settings(
        scalacOptions ++= List(
          "-language:higherKinds",
          "-Xlint:_,-missing-interpolator"
        ),
        description := "Principled code generation for Scala services from OpenAPI specifications",
        homepage := Some(url("https://github.com/guardrail-dev/guardrail")),
        scmInfo := Some(
          ScmInfo(
            url("https://github.com/guardrail-dev/guardrail"),
            "scm:git@github.com:guardrail-dev/guardrail.git"
          )
        ),
        developers := List(
          Developer(
            id = "blast_hardcheese",
            name = "Devon Stewart",
            email = "blast@hardchee.se",
            url = url("http://hardchee.se/")
          )
        )
      )
      .settings(
        scalacOptions ++= ifScalaVersion(_ <= 11)(List("-Xlint:-missing-interpolator,_")).value,
        scalacOptions ++= ifScalaVersion(_ >= 12)(List("-Xlint:-unused,-missing-interpolator,_")).value,
        scalacOptions ++= ifScalaVersion(_ == 12)(List("-Ypartial-unification", "-Ywarn-unused-import")).value,
        scalacOptions ++= ifScalaVersion(_ >= 13)(List("-Ywarn-unused:imports")).value,
      )

  implicit class ProjectSyntax(project: Project) {
    // Adding these to I _think_ work around https://github.com/sbt/sbt/issues/3733 ?
    // Seems like there's probably a better way to do this, but I don't know what it is
    // The intent is we should be able to use `dependsOn(core % Provided)` to compile
    // against the module's classes, but then emit <scope>provided</scope> in the published POM.
    //
    // Currently, it seems as though `dependsOn(core % Provided)` doesn't expose
    // classes from `core` to the depending module, which means even though we get the desired
    // scope in the pom, it's useless since we can't actually compile the project.
    def accumulateClasspath(other: Project): Project =
      project
        .settings(Compile / unmanagedClasspath := {
          val current = (Compile / unmanagedClasspath).value
          val fromOther = (other / Compile / fullClasspathAsJars).value
          (current ++ fromOther).distinct
        })
        .settings(Runtime / unmanagedClasspath := {
          val current = (Runtime / unmanagedClasspath).value
          val fromOther = (other / Runtime / fullClasspathAsJars).value
          (current ++ fromOther).distinct
        })
        .settings(Test / unmanagedClasspath := {
          val current = (Test / unmanagedClasspath).value
          val fromOther = (other / Test / fullClasspathAsJars).value
          (current ++ fromOther).distinct ++ (other / Test / exportedProductJars).value
        })
        .settings(Default / unmanagedClasspath := {
          val current = (Compile / unmanagedClasspath).value
          val fromOther = (other / Compile / fullClasspathAsJars).value
          (current ++ fromOther).distinct
        })

    def customDependsOn(other: Project, useProvided: Boolean = false): Project = {
      if (useStableVersions) {
        project
          .settings(libraryDependencySchemes += "dev.guardrail" % other.id % "early-semver")
          .settings(libraryDependencies += {
            val base = "dev.guardrail" %% other.id % (other / stableVersion).value
            if (useProvided) base % Provided else base
          })
          .settings(
            // dependsOn(other % Test) adds the undesirable dependsOn(other % Compile) as a side-effect.
            // Mirror libraryDependencies and source directory tracking to approximate test dependencies
            Test / libraryDependencies ++= (other / Test / libraryDependencies).value,
            // Add everything from `other`'s test scope to our classpath
            Test / unmanagedJars ++= (other / Test / exportedProductJars).value,
            // Carry along `other`'s exportedProductJars along in ours, so subsequent projects can depend on them
            Test / exportedProductJars := (Test / exportedProductJars).value ++ (other / Test / exportedProductJars).value
          )
      } else {
        project
          .dependsOn(other)
          .accumulateClasspath(other)
      }
    }

    def providedDependsOn(other: Project): Project =
      customDependsOn(other, true)
  }
}
