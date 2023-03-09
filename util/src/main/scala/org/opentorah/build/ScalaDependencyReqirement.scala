package org.opentorah.build

import org.gradle.api.Project

open class ScalaDependencyRequirement(
  dependency: ScalaDependency,
  version: String,
  scalaLibrary: ScalaLibrary,
  reason: String,
  configurations: Configurations,
  isVersionExact: Boolean = false
) extends DependencyRequirement(
  dependency,
  version,
  reason,
  configurations,
  isVersionExact
):
  override protected def getAddition(project: Project): Dependency.WithVersion =
    val scalaVersion: String = dependency match
      case _: Scala2Dependency => scalaLibrary.getScala2Version
      case _: Scala3Dependency => scalaLibrary.getScala3Version

    dependency.apply(
      scalaVersion,
      version
    )

  override protected def verify(found: Dependency.WithVersion, project: Project): Unit =
  // TODO fix it!
  //    if found.dependency.isInstanceOf[Scala2Dependency] then // TODO get rid of casts
  //      val scalaVersion: String = getScalaVersion(found.dependency.asInstanceOf[Scala2Dependency])
  //      val version: Scala2Dependency.Version = found.version.asInstanceOf[Scala2Dependency.Version]
  //      if version.scalaVersion != scalaVersion then project.getLogger.info(
  //        s"Found $found, but the project uses Scala 2 version $scalaVersion", null, null, null
  //      )

    super.verify(found, project)

