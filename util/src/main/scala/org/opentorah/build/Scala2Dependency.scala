package org.opentorah.build

open class Scala2Dependency(
  group: String,
  artifact: String,
  val isScala2versionFull: Boolean = false
) extends ScalaDependency(
  group = group,
  artifact = artifact
):
  final override protected def versionSuffix(scalaVersion: String): String =
    if isScala2versionFull
    then scalaVersion
    else scalaVersion.split('.').take(2).mkString(".")
