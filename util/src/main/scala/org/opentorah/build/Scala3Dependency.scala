package org.opentorah.build

open class Scala3Dependency(
  group: String,
  artifact: String
) extends ScalaDependency(
  group = group,
  artifact = artifact
):
  final override protected def versionSuffix(scalaVersion: String): String = "3"


