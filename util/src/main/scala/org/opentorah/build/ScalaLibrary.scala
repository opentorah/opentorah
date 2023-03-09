package org.opentorah.build

import org.gradle.api.artifacts.Configuration
import java.io.File

final class ScalaLibrary(
  val scala3: Option[Dependency.WithVersion],
  val scala2: Option[Dependency.WithVersion]
):
  require(scala3.nonEmpty || scala2.nonEmpty, "No Scala library!")
  def isScala3: Boolean = scala3.nonEmpty
  def isScala2: Boolean = scala3.isEmpty

  override def toString: String = s"ScalaLibrary(scala3=${scala3.map(_.version)}, scala2=${scala2.map(_.version)})"

  def getScala2Version: String = scala2.map(_.version.version)
    .getOrElse(ScalaLibrary.scala2version(scala3.get.version.version))

  def getScala3Version: String = scala3.get.version.version

  def verify(other: ScalaLibrary): Unit =
    require(
      other.isScala3 == isScala3,
      s"Scala 3 presence changed from $isScala3 to ${other.isScala3}"
    )
    if isScala3
    then require(
      other.scala3.get.version.version == scala3.get.version.version,
      s"Scala 3 version changed from ${scala3.get.version.version} to ${other.scala3.get.version.version}"
    )
    else require(
      other.scala2.get.version.version == scala2.get.version.version,
      s"Scala 2 version changed from ${scala2.get.version.version} to ${other.scala2.get.version.version}"
    )

object ScalaLibrary:
  val group: String = "org.scala-lang"

  object Scala2 extends JavaDependency(group = group, artifact = "scala-library" )
  object Scala3 extends JavaDependency(group = group, artifact = "scala3-library_3")

  // Note: there is no Scala 2 equivalent
  // TODO introduce platformSuffix
  object Scala3SJS extends Scala3Dependency(group = group, artifact = "scala3-library_sjs1")

  // Note: Scala 2 minor version used by Scala 3 from 3.0.0 to the current is 2.13
  private def scala2version(scala3version: String): String = "2.13"

  def getFromConfiguration(configuration: Configuration): ScalaLibrary =
    ScalaLibrary(
      scala3 = Scala3.getFromConfiguration(configuration),
      scala2 = Scala2.getFromConfiguration(configuration)
    )

  def getFromClasspath(classPath: Iterable[File]): ScalaLibrary =
    val result: ScalaLibrary = ScalaLibrary(
      scala3 = Scala3.getFromClassPath(classPath),
      scala2 = Scala2.getFromClassPath(classPath)
    )
    require(result.scala2.nonEmpty, "No Scala 2 library!")
    result
