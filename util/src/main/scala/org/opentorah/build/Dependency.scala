package org.opentorah.build

import org.gradle.api.artifacts.{Configuration, Dependency as GDependency}
import org.opentorah.util.Strings
import java.io.File
import java.util.regex.{Matcher, Pattern}
import Gradle.*
import scala.jdk.CollectionConverters.*

abstract class DependencyVersion(
  val dependency: Dependency,
  val version: String
):
  final override def toString: String = s"'$dependencyNotation'"

  final def dependencyNotation: String = s"${dependency.group}:$nameDependencyNotation:$version"

  def nameDependencyNotation: String

object DependencyVersion:
  def getMajor(version: String): String = Strings.splitRight(version, '.')._1.get

abstract class Dependency(
  val group: String,
  val nameBase: String
):
  protected def namePattern: String

  def getFromConfiguration(configuration: Configuration): Option[DependencyVersion] =
    val patternCompiled: Pattern = Pattern.compile(namePattern)
    val result: Set[DependencyVersion] = for
      dependency: GDependency <- configuration.getDependencies.asScala.toSet
      if dependency.getGroup == group
      matcher: Matcher = patternCompiled.matcher(dependency.getName)
      if matcher.matches
    yield
      fromMatcher(matcher, dependency.getVersion)
    result.headOption

  protected def fromMatcher(matcher: Matcher, version: String): DependencyVersion

  def getFromClassPath(classPath: Iterable[File]): Option[DependencyVersion] =
    val patternCompiled: Pattern = Pattern.compile(s"$namePattern-(\\d.*).jar")
    val result: Iterable[DependencyVersion] = for
      file: File <- classPath
      matcher: Matcher = patternCompiled.matcher(file.getName)
      if matcher.matches
    yield
      fromMatcher(matcher)
    result.headOption

  protected def fromMatcher(matcher: Matcher): DependencyVersion
