package org.opentorah.build

import org.gradle.api.{Project, Task, UnknownDomainObjectException}
import org.gradle.api.plugins.JavaPluginExtension
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.SourceSet
import scala.jdk.CollectionConverters.*

object Gradle:
  extension(project: Project)
    def findExtension[T](clazz: Class[T]): Option[T] =
      Option(project.getExtensions.findByType(clazz))

    def getExtension[T](clazz: Class[T]): T =
      project.getExtensions.getByType(clazz)

    def findSourceSet(name: String): Option[SourceSet] =
      project.findExtension(classOf[JavaPluginExtension])
      .map(_.getSourceSets.getByName(name))

    def getSourceSet(name: String): SourceSet =
      project
      .getExtension(classOf[JavaPluginExtension])
      .getSourceSets.getByName(name)

    def findMainSourceSet: Option[SourceSet] =
      findSourceSet(SourceSet.MAIN_SOURCE_SET_NAME)

    def getMainSourceSet: SourceSet =
      getSourceSet(SourceSet.MAIN_SOURCE_SET_NAME)

    def findTask(name: String): Option[Task] =
      Option(project.getTasks.findByName(name))

    def getTask(name: String): Task =
      project.getTasks.getByName(name)

    def findClassesTask: Option[Task] =
      project.findMainSourceSet.flatMap(sourceSet => project.findTask(sourceSet.getClassesTaskName))

    def getClassesTask(sourceSet: SourceSet): Task = project.getTask(sourceSet.getClassesTaskName)

  extension[T](property: Property[T])
    def toOption: Option[T] =
      if !property.isPresent then None else Some(property.get)

    def getOrElse(default: => T): T =
      if !property.isPresent then default else property.get

  extension[T](property: Property[String])
    def byName(default: => T, all: List[T]): T =
      if !property.isPresent then default else all.find(_.toString == property.get).get

  extension[T](property: ListProperty[T])
    def toList: List[T] =
      property.get().asScala.toList

  extension(property: MapProperty[String, String])
    def toMap: Map[String, String] =
      property.get().asScala.toMap
