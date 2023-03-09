package org.opentorah.build

import org.gradle.api.artifacts.{Configuration, Dependency as GDependency}
import org.opentorah.util.Strings
import java.io.File
import java.util.regex.{Matcher, Pattern}
import scala.jdk.CollectionConverters.*

abstract class Dependency(
  val group: String,
  val artifact: String
):
  type Version <: Dependency.Version

  final def artifactWithSuffix(version: Version): String = artifact + artifactSuffix(version)
  
  protected def artifactSuffix(version: Version): String

  private def artifactPattern: String = artifact + artifactSuffixPattern

  protected def artifactSuffixPattern: String

  final def getFromConfiguration(configuration: Configuration): Option[Dependency.WithVersion] =
    val patternCompiled: Pattern = Pattern.compile(artifactPattern)
    val result: Set[Dependency.WithVersion] = for
      dependency: GDependency <- configuration.getDependencies.asScala.toSet
      if dependency.getGroup == group
      matcher: Matcher = patternCompiled.matcher(dependency.getName)
      if matcher.matches
    yield
      fromMatcher(matcher, dependency.getVersion)
    result.headOption

  protected def fromMatcher(matcher: Matcher, version: String): Dependency.WithVersion

  final def getFromClassPath(classPath: Iterable[File]): Option[Dependency.WithVersion] =
    val patternCompiled: Pattern = Pattern.compile(s"$artifactPattern-(\\d.*).jar")
    val result: Iterable[Dependency.WithVersion] = for
      file: File <- classPath
      matcher: Matcher = patternCompiled.matcher(file.getName)
      if matcher.matches
    yield
      fromMatcher(matcher)
    result.headOption

  protected def fromMatcher(matcher: Matcher): Dependency.WithVersion

  final def dependencyNotation(version: Version): String =
    val artifactStr: String = artifactWithSuffix(version)
    val classifierStr: String = Strings.prefix(":", classifier(version))
    val extensionStr: String = Strings.prefix("@", extension(version))
    s"$group:$artifactStr:${version.version}$classifierStr$extensionStr"

  def classifier(version: Version): Option[String] = None

  def extension(version: Version): Option[String] = None

object Dependency:
  open class Version(val version: String)

  class WithVersion(
    val dependency: Dependency,
    val version: dependency.Version
  ):
    override def toString: String = s"'$dependencyNotation'"

    def dependencyNotation: String = dependency.dependencyNotation(version)
