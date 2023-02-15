package org.opentorah.build

import org.gradle.api.{Project, Task}
import org.gradle.api.artifacts.Configuration
import org.gradle.api.logging.{Logger, LogLevel}
import org.gradle.api.plugins.JavaPluginExtension
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{SourceSet, TaskProvider}
import org.gradle.api.tasks.scala.ScalaCompile
import org.gradle.internal.classloader.{ClassLoaderVisitor, ClasspathUtil, VisitableURLClassLoader}
import org.opentorah.util.Files
import java.io.File
import java.net.{URL, URLClassLoader}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object Gradle:
  def getLogLevelEnabled(logger: Logger): LogLevel = levels.find(logger.isEnabled).get

  private val levels: Seq[LogLevel] = Seq(
    LogLevel.DEBUG,
    LogLevel.INFO,
    LogLevel.LIFECYCLE,
    LogLevel.WARN,
    LogLevel.QUIET,
    LogLevel.ERROR
  )

  extension(project: Project)
    def findExtension[T](clazz: Class[T]): Option[T] =
      Option(project.getExtensions.findByType(clazz))

    def getExtension[T](clazz: Class[T]): T =
      project.getExtensions.getByType(clazz)

    def getConfiguration(name: String): Configuration =
      project.getConfigurations.getByName(name)

    def getScalaCompile(sourceSet: SourceSet): ScalaCompile =
      project.getClassesTask(sourceSet).getScalaCompile

    def getScalaCompile(sourceSetName: String): ScalaCompile =
      project.getScalaCompile(project.getSourceSet(sourceSetName))

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
      property.get.asScala.toList

  extension(property: MapProperty[String, String])
    def toMap: Map[String, String] =
      property.get.asScala.toMap

//  private val addUrlMethod: Method = classOf[URLClassLoader].getDeclaredMethod("addURL", classOf[URL])
//  addUrlMethod.setAccessible(true)

  def addToClassPath(obj: AnyRef, files: Iterable[File]): ClassLoader =
    val result: ClassLoader = obj.getClass.getClassLoader

    val urls: Iterable[URL] = files.map(_.toURI.toURL)

    result match
      case visitable: VisitableURLClassLoader =>
        for url <- urls do visitable.addURL(url)
      case classLoader =>
        ClasspathUtil.addUrl(
          classLoader.asInstanceOf[URLClassLoader],
          urls.asJava
        )

    result

  def findOnClassPath(obj: AnyRef, name: String): URL =
    var result: Option[URL] = None

    val visitor: ClassLoaderVisitor = new ClassLoaderVisitor:
      override def visitClassPath(classPath: Array[URL]): Unit = classPath
        .find(_.getPath.contains(name))
        .foreach((url: URL) => result = Some(url))

    visitor.visit(obj.getClass.getClassLoader)

    result.getOrElse(throw IllegalArgumentException(s"Did not find artifact $name"))

  def collectClassPath(classLoader: ClassLoader): Seq[File] =
    val result: mutable.ArrayBuffer[File] = mutable.ArrayBuffer.empty[File]
    val visitor: ClassLoaderVisitor = new ClassLoaderVisitor:
      override def visitClassPath(classPath: Array[URL]): Unit =
        for
          url <- classPath
          if url.getProtocol == "file"
        do
          result += Files.url2file(url)
    visitor.visit(classLoader)
    result.toSeq
