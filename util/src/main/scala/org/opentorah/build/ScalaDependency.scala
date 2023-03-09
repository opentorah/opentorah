package org.opentorah.build

import java.util.regex.Matcher

abstract class ScalaDependency(
  group: String,
  artifact: String
) extends Dependency(
  group = group,
  artifact = artifact
):
  final override type Version = ScalaDependency.Version

  final override protected def artifactSuffix(version: Version): String = "_" + versionSuffix(version.scalaVersion)

  protected def versionSuffix(scalaVersion: String): String

  final override protected def artifactSuffixPattern: String = "_(\\d.*)"

  final override protected def fromMatcher(matcher: Matcher, version: String): Dependency.WithVersion = apply(
    scalaVersion = matcher.group(1),
    version = version
  )

  final override protected def fromMatcher(matcher: Matcher): Dependency.WithVersion = apply(
    scalaVersion = matcher.group(1),
    version = matcher.group(2)
  )

  final def apply(
    scalaVersion: String,
    version: String
  ): Dependency.WithVersion = Dependency.WithVersion(
    dependency = this,
    version = ScalaDependency.Version(
      scalaVersion = scalaVersion,
      version = version
    )
  )

object ScalaDependency:
  final class Version(
    version: String,
    val scalaVersion: String
  ) extends Dependency.Version(
    version
  )
