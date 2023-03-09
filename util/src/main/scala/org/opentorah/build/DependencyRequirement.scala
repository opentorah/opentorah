package org.opentorah.build

import org.gradle.api.artifacts.Configuration
import org.gradle.api.{GradleException, Project}
import Gradle.*
import scala.jdk.CollectionConverters.*

abstract class DependencyRequirement(
  dependency: Dependency,
  version: String,
  reason: String,
  configurations: Configurations,
  isVersionExact: Boolean = false
):
  // Note: all applyToConfiguration() must be run first: once a applyToClassPath() runs,
  // configuration is no longer changeable.
  final def applyToConfiguration(project: Project): Dependency.WithVersion =
    val found: Option[Dependency.WithVersion] = dependency
      .getFromConfiguration(project.getConfiguration(configurations.configuration))
    found.foreach(verify(_, project))
    found.getOrElse {
      val configuration: Configuration = getConfiguration(project)
      val toAdd: Dependency.WithVersion = getAddition(project)
      project.getLogger.info(s"Adding dependency $toAdd to the $configuration $reason", null, null, null)
      configuration
        .getDependencies
        .add(project.getDependencies.create(toAdd.dependencyNotation))
      toAdd
    }

  // Note: Once a classpath is resolved (e.g., by enumerating JARs on it :)),
  // dependencies can not be added to the configurations involved:
  //   Cannot change dependencies of dependency configuration ... after it has been included in dependency resolution
  // So the only thing we can do is to report the missing dependency:
  final def applyToClassPath(project: Project): Dependency.WithVersion =
    val found: Option[Dependency.WithVersion] = dependency.getFromClassPath(
      project.getConfiguration(configurations.classPath).asScala
    )
    found.foreach(verify(_, project))
    found.getOrElse {
      val configuration: Configuration = getConfiguration(project)
      val toAdd: Dependency.WithVersion = getAddition(project)
      throw GradleException(s"Please add dependency $toAdd to the $configuration $reason")
    }

  protected def verify(found: Dependency.WithVersion, project: Project): Unit =
    if isVersionExact && found.version.version != version then project.getLogger.info(
      s"Found $found, but the project uses version $version", null, null, null
    )

  private def getConfiguration(project: Project): Configuration = project.getConfiguration(configurations.configuration)
  
  protected def getAddition(project: Project): Dependency.WithVersion

object DependencyRequirement:
  // Note: all applyToConfiguration() must be run first: once a applyToClassPath() runs,
  // configuration is no longer changeable.
  def applyToProject(requirements: Seq[DependencyRequirement], project: Project): Unit =
    requirements.foreach(_.applyToConfiguration(project))
    requirements.foreach(_.applyToClassPath(project))
