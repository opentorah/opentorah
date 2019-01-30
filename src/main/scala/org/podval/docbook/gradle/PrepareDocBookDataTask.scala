package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.plugins.JavaPluginConvention
import org.gradle.api.provider.Property
import org.gradle.api.tasks.{Input, JavaExec, OutputDirectory}

import scala.beans.BeanProperty

class PrepareDocBookDataTask extends JavaExec {
  @Input @BeanProperty val dataGeneratorClass: Property[String] =
    getProject.getObjects.property(classOf[String])

  @OutputDirectory @BeanProperty val dataDirectory: Property[File] =
    getProject.getObjects.property(classOf[File])

  override def exec(): Unit = {
    val javaPlugin: JavaPluginConvention = getProject.getConvention.findPlugin(classOf[JavaPluginConvention])
    if (javaPlugin == null) {
      info("Skipping DocBook data generation: no Java plugin in the project")
    } else {
      val mainClass: String = dataGeneratorClass.get
      if (mainClass.isEmpty) {
        info("Skipping DocBook data generation: dataGenerationClass is not set")
      } else {
        val outputDirectory = dataDirectory.get
        info(s"Running DocBook data generator $mainClass into $outputDirectory")
        setClasspath(javaPlugin.getSourceSets.findByName("main").getRuntimeClasspath)
        setMain(mainClass)
        setArgsString(outputDirectory.toString)
        super.exec()
      }
    }
  }

  private def info(message: String): Unit =
    getLogger.info(message, null, null)
}
