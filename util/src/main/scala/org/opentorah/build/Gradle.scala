package org.opentorah.build

import org.gradle.api.{Project, Task}
import org.gradle.api.artifacts.Configuration
import org.gradle.api.plugins.JavaPluginExtension
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{SourceSet, TaskProvider}
import org.gradle.api.tasks.scala.ScalaCompile
import java.lang.reflect.Method
import java.net.{URL, URLClassLoader}
import scala.jdk.CollectionConverters.*

object Gradle:
  extension(project: Project)
    def findExtension[T](clazz: Class[T]): Option[T] =
      Option(project.getExtensions.findByType(clazz))

    def getExtension[T](clazz: Class[T]): T =
      project.getExtensions.getByType(clazz)

    def getConfiguration(name: String): Configuration =
      project.getConfigurations.getByName(name)

    def getScalaCompile(sourceSet: SourceSet): ScalaCompile =
      project.getClassesTask(sourceSet).getScalaCompile

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

  extension(classesTask: Task)
    def getScalaCompile: ScalaCompile = classesTask
      .getDependsOn
      .asScala
      .find(classOf[TaskProvider[ScalaCompile]].isInstance)
      .get
      .asInstanceOf[TaskProvider[ScalaCompile]]
      .get

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

  private val addUrlMethod: Method = classOf[URLClassLoader].getDeclaredMethod("addURL", classOf[URL])
  addUrlMethod.setAccessible(true)

  def addConfigurationToClassPath(task: Task, configurationName: String): Unit =
    val classLoader: URLClassLoader = task.getClass.getClassLoader.asInstanceOf[URLClassLoader]
    task
      .getProject
      .getConfiguration(configurationName)
      .asScala
      .map(_.toURI.toURL)
      .foreach((url: URL) => addUrlMethod.invoke(classLoader, url))
