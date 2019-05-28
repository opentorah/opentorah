package org.podval.docbook.gradle.util

import java.io.File

import org.gradle.api.{Action, Project}
import org.gradle.api.artifacts.repositories.{ArtifactRepository, IvyArtifactRepository, IvyPatternRepositoryLayout}
import org.gradle.api.artifacts.{Configuration, Dependency}
import org.gradle.api.file.{CopySpec, FileCopyDetails, RelativePath}

object Gradle {

  final case class Repository(
    url: String,
    artifact: String,
    ivy: String
  )

  def getArtifact(project: Project, dependencyNotation: String): File = {
    val dependency: Dependency = project.getDependencies.create(dependencyNotation)
    val configuration: Configuration = project.getConfigurations.detachedConfiguration(dependency)
    configuration.setTransitive(false)
    configuration.getSingleFile
  }

  def getArtifact(project: Project, dependencyNotation: String, newRepository: Repository): File = {
    // Stash all the repositories
    val allRepositories: java.util.List[ArtifactRepository] = new java.util.ArrayList[ArtifactRepository]()
    allRepositories.addAll(project.getRepositories)
    project.getRepositories.clear()

    // Add Node repository
    project.getRepositories.ivy(new Action[IvyArtifactRepository] {
      override def execute(repository: IvyArtifactRepository): Unit = {
        repository.setUrl(newRepository.url)
        repository.layout("pattern", new Action[IvyPatternRepositoryLayout] { // TODO patternLayout
          override def execute(repositoryLayout: IvyPatternRepositoryLayout): Unit = {
            repositoryLayout.artifact(newRepository.artifact)
            repositoryLayout.ivy(newRepository.ivy)
          }
        })
      }
    })

    // Resolve the dependency
    val result: File = getArtifact(project, dependencyNotation)

    // Restore original repositories
    project.getRepositories.clear()
    project.getRepositories.addAll(allRepositories)

    result
  }

  def unpack(project: Project, archiveFile: File, isZip: Boolean, directory: File): Unit = {
    project.copy((copySpec: CopySpec) => copySpec
      .from(if (isZip) project.zipTree(archiveFile) else project.tarTree(archiveFile))
      .into(directory)
    )
  }

  def unpack(project: Project, zipFile: File, archiveSubdirectoryName: String, directory: File): Unit = {
    val toDrop: Int = archiveSubdirectoryName.count(_ == '/') + 1
    project.copy((copySpec: CopySpec) => copySpec
      .into(directory)
      .from(project.zipTree(zipFile))
      // TODO following code deals with extracting just the "docbook" directory;
      // this should become easier in Gradle 5.3, see:
      // https://github.com/gradle/gradle/issues/1108
      // https://github.com/gradle/gradle/pull/5405
      .include(archiveSubdirectoryName + "/**")
      .eachFile((file: FileCopyDetails) =>
        file.setRelativePath(new RelativePath(true, file.getRelativePath.getSegments.drop(toDrop): _*))
      )
      .setIncludeEmptyDirs(false))
  }
}
