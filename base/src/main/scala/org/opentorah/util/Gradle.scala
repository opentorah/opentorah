package org.opentorah.util

import java.io.File
import org.apache.tools.ant.filters.ReplaceTokens
import org.gradle.api.artifacts.repositories.{ArtifactRepository, IvyArtifactRepository, IvyPatternRepositoryLayout}
import org.gradle.api.artifacts.{Configuration, Dependency}
import org.gradle.api.file.{CopySpec, FileCollection}
import org.gradle.api.plugins.JavaPluginConvention // TODO update to JavaPluginExtension
import org.gradle.api.tasks.SourceSet
import org.gradle.api.{Action, Project, Task}
import org.gradle.process.{ExecResult, JavaExecSpec}
import org.slf4j.{Logger, LoggerFactory}
import scala.jdk.CollectionConverters._

object Gradle {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def getArtifact(project: Project, dependencyNotation: String): Option[File] = {
    logger.info(s"Resolving $dependencyNotation")

    val dependency: Dependency = project.getDependencies.create(dependencyNotation)
    val configuration: Configuration = project.getConfigurations.detachedConfiguration(dependency)
    configuration.setTransitive(false)

    try {
      val result: File = configuration.getSingleFile
      logger.info(s"Resolved: $result")
      Some(result)
    } catch {
      case _: IllegalStateException =>
        logger.warn(s"Failed to resolve: $dependencyNotation")
        None
    }
  }

  def getArtifact(
    project: Project,
    repositoryUrl: String,
    artifactPattern: String,
    ivy: String,
    dependencyNotation: String,
  ): File = {
    // Stash all the repositories
    val allRepositories: java.util.List[ArtifactRepository] = new java.util.ArrayList[ArtifactRepository]()
    allRepositories.addAll(project.getRepositories)
    project.getRepositories.clear()

    // Add repository
    project.getRepositories.ivy((repository: IvyArtifactRepository) => {
      repository.setUrl(repositoryUrl)
      repository.patternLayout((repositoryLayout: IvyPatternRepositoryLayout) => {
        repositoryLayout.artifact(artifactPattern)
        repositoryLayout.ivy(ivy)
      })

      // Gradle 6.0 broke NodeJS retrieval;
      // from https://github.com/gradle/gradle/issues/11006 and code referenced there
      // https://github.com/gradle/gradle/blob/b189979845c591d8c4a0032527383df0f6d679b2/subprojects/javascript/src/main/java/org/gradle/plugins/javascript/base/JavaScriptRepositoriesExtension.java#L53
      // it seems that to re-gain Gradle 5.6 behaviour, this needs to be done:
      repository.metadataSources((metadataSources: IvyArtifactRepository.MetadataSources) => {
        metadataSources.artifact(); // Indicates that this repository may not contain metadata files...
      })
    })

    // Resolve the dependency
    val result: File = getArtifact(project, dependencyNotation).get

    // Restore original repositories
    project.getRepositories.clear()
    project.getRepositories.addAll(allRepositories)

    result
  }

  def unpack(project: Project, archiveFile: File, isZip: Boolean, into: File): Unit = {
    logger.info(s"Unpacking $archiveFile into $into")

    into.mkdir()
    project.copy(new Action[CopySpec] {
      override def execute(copySpec: CopySpec): Unit = copySpec
        .from(if (isZip) project.zipTree(archiveFile) else project.tarTree(archiveFile))
        .into(into)
    })
  }

  def copyDirectory(
    project: Project,
    into: File,
    from: File,
    directoryName: String,
    substitutions: Map[String, String] = Map.empty
  ): Unit = project.copy((copySpec: CopySpec) => {
    copySpec
      .into(into)
      .from(from)
      .include(directoryName + "/**")

    if (substitutions.nonEmpty)
      copySpec.filter(Map("tokens" -> substitutions.asJava).asJava, classOf[ReplaceTokens])
  })

  private def getTask(project: Project, name: String): Option[Task] = Option(project.getTasks.findByName(name))

  def getClassesTask(project: Project): Option[Task] =
    mainSourceSet(project).flatMap(mainSourceSet => getTask(project, mainSourceSet.getClassesTaskName))

  def mainSourceSet(project: Project): Option[SourceSet] =
    Option(project.getConvention.findPlugin(classOf[JavaPluginConvention]))
      .map(_.getSourceSets.getByName(SourceSet.MAIN_SOURCE_SET_NAME))

  def javaexec(
    project: Project,
    mainClass: String,
    classpath: FileCollection,
    args: String*
  ): ExecResult = project.javaexec(new Action[JavaExecSpec]{
    override def execute(exec: JavaExecSpec): Unit = {
      exec.setClasspath(classpath)
      exec.getMainClass.set(mainClass)
      exec.args(args: _*)
    }
  })
}
