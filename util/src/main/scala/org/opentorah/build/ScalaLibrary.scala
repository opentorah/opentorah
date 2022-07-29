package org.opentorah.build

import org.gradle.api.artifacts.Configuration
import java.io.File

final class ScalaLibrary(
  val scala3: Option[DependencyVersion],
  val scala2: Option[DependencyVersion]
):
  require(scala3.nonEmpty || scala2.nonEmpty, "No Scala library!")
  def isScala3: Boolean = scala3.nonEmpty
  def isScala2: Boolean = scala3.isEmpty

  override def toString: String = s"ScalaLibrary(scala3=${scala3.map(_.version)}, scala2=${scala2.map(_.version)})"

  def verify(other: ScalaLibrary): Unit =
    require(
      other.isScala3 == isScala3,
      s"Scala 3 presence changed from $isScala3 to ${other.isScala3}"
    )
    if isScala3
    then require(
      other.scala3.get.version == scala3.get.version,
      s"Scala 3 version changed from ${scala3.get.version} to ${other.scala3.get.version}"
    )
    else require(
      other.scala2.get.version == scala2.get.version,
      s"Scala 2 version changed from ${scala2.get.version} to ${other.scala2.get.version}"
    )

object ScalaLibrary:
  val group: String = "org.scala-lang"

  object Scala2 extends SimpleDependency(group = group, nameBase = "scala-library")
  object Scala3 extends Scala3Dependency(group = group, nameBase = "scala3-library")

  // Note: there is no Scala 2 equivalent
  object Scala3SJS extends Scala3Dependency(group = group, nameBase = "scala3-library_sjs1")

  // Note: Scala 2 minor version used by Scala 3 from 3.0.0 to the current is 2.13
  def scala2versionMinor(scala3version: String): String = "2.13"

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
