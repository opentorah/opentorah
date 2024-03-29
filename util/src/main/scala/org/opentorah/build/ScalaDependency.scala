package org.opentorah.build

import org.gradle.api.Project
import org.opentorah.util.Strings

final class ScalaDependency(
  val findable: ScalaDependency.Findable,
  val scalaVersion: Version
) extends Dependency:
  override def group: String = findable.group
  override def artifact: String = findable.artifact
  override def classifier(version: Version): Option[String] = findable.classifier(version)
  override def extension(version: Version): Option[String] = findable.extension(version)

  override def artifactName: String =
    s"${artifact}${findable.platformSuffix}_${findable.versionSuffix(scalaVersion)}"

object ScalaDependency:

  abstract class Findable(
    override val group: String,
    override val artifact: String,
    val isScalaJS: Boolean
  ) extends Dependency.Findable:

    final override def classifier(version: Version): Option[String] = None

    final override def extension(version: Version): Option[String] = None

    override def dependencyForArtifactName(artifactName: String): Option[ScalaDependency] =
      val (artifactAndPlatform: String, scalaVersionOpt: Option[String]) = Strings.split(artifactName, '_')
      val (artifact: String, platformOpt: Option[String]) = Strings.split(artifactAndPlatform, '_')
      val recognized: Boolean =
        scalaVersionOpt.isDefined &&
        (artifact == this.artifact) &&
        (!isScalaJS || platformOpt.contains("sjs1"))
      if !recognized then None else
        val scalaVersion: Version = Version(scalaVersionOpt.get)
        if !isScalaVersionAcceptable(scalaVersion) then None else Some(withScalaVersion(scalaVersion))

    private def isScalaVersionAcceptable(scalaVersion: Version): Boolean =
      (scalaVersion.major == scala.versionMajor) &&
      isScalaVersionOfCorrectLength(scalaVersion)

    final def platformSuffix: String = if !isScalaJS then "" else "_sjs1"

    final def withScalaVersion(scalaVersion: Version): ScalaDependency =
      require(isScalaVersionAcceptable(scalaVersion), s"Scala version $scalaVersion is not acceptable: $this requires $scala")
      ScalaDependency(
        this,
        scalaVersion
      )

    def scala: ScalaLibrary.Scala

    def isScalaVersionOfCorrectLength(scalaVersion: Version): Boolean

    def versionSuffix(scalaVersion: Version): String

  open class Scala2(
    group: String,
    artifact: String,
    isScalaJS: Boolean = false,
    isScalaVersionFull: Boolean = false
  ) extends Findable(
    group = group,
    artifact = artifact,
    isScalaJS = isScalaJS
  ):
    final override def scala: ScalaLibrary.Scala = ScalaLibrary.Scala2

    final override def isScalaVersionOfCorrectLength(scalaVersion: Version): Boolean = true

    final override def versionSuffix(scalaVersion: Version): String =
      if isScalaVersionFull
      then scalaVersion.version
      else scalaVersion.majorAndMinorString

  open class Scala3(
    group: String,
    artifact: String,
    isScalaJS: Boolean = false
  ) extends Findable(
    group = group,
    artifact = artifact,
    isScalaJS = isScalaJS
  ):
    final override def scala: ScalaLibrary.Scala = ScalaLibrary.Scala3

    final override def isScalaVersionOfCorrectLength(scalaVersion: Version): Boolean = true

    final override def versionSuffix(scalaVersion: Version): String =
      scalaVersion.major.toString

  open class Requirement(
    findable: Findable,
    version: Version,
    scalaLibrary: ScalaLibrary,
    reason: String,
    configurations: Configurations,
    isVersionExact: Boolean = false
  ) extends DependencyRequirement(
    findable,
    version,
    reason,
    configurations,
    isVersionExact
  ):
    override protected def getDependency: Dependency =
      findable.withScalaVersion(findable.scala.getScalaVersion(scalaLibrary))

    override protected def verify(found: Dependency.WithVersion, project: Project): Unit =
    //    if found.dependency.isInstanceOf[Scala2Dependency] then // TODO get rid of casts
    //      val scalaVersion: String = getScalaVersion(found.dependency.asInstanceOf[Scala2Dependency])
    //      val version: Scala2Dependency.Version = found.version.asInstanceOf[Scala2Dependency.Version]
    //      if version.scalaVersion != scalaVersion then project.getLogger.info(
    //        s"Found $found, but the project uses Scala 2 version $scalaVersion", null, null, null
    //      )

      super.verify(found, project)
