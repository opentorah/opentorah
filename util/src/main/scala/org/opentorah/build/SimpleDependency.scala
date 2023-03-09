package org.opentorah.build

import java.util.regex.Matcher

abstract class SimpleDependency(
  group: String,
  artifact: String
) extends Dependency(
  group = group,
  artifact = artifact
):
  override type Version = Dependency.Version

  final override protected def artifactSuffix(version: Version): String = ""

  final override protected def artifactSuffixPattern: String = ""

  final override protected def fromMatcher(matcher: Matcher, version: String): Dependency.WithVersion = apply(
    version = version
  )

  final override protected def fromMatcher(matcher: Matcher): Dependency.WithVersion = apply(
    version = matcher.group(1)
  )

  def apply(
    version: String
  ): Dependency.WithVersion = Dependency.WithVersion(
    dependency = this,
    version = Dependency.Version(version)
  )
