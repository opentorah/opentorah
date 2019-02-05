package org.podval.docbook.gradle

import org.gradle.api.provider.Property
import org.gradle.api.tasks.{Input, JavaExec, OutputDirectory}
import java.io.File
import scala.beans.BeanProperty

class PrepareDocBookDataTask extends JavaExec {
  val layout: Layout = new Layout(getProject)
  val logger: Logger = new Logger.PluginLogger(getProject.getLogger)

  @Input @BeanProperty val dataGeneratorClass: Property[String] =
    getProject.getObjects.property(classOf[String])

  @OutputDirectory val dataDirectory: File = layout.dataDirectory

  override def exec(): Unit = {
    val mainClass: String = dataGeneratorClass.get
    if (mainClass.isEmpty) {
      logger.info("Skipping DocBook data generation: dataGenerationClass is not set")
    } else {
      if (layout.mainSourceSet.isEmpty) {
        logger.info("Skipping DocBook data generation: no Java plugin in the project")
      } else {
        val outputDirectory = dataDirectory
        logger.info(s"Running DocBook data generator $mainClass into $outputDirectory")
        setClasspath(layout.mainSourceSet.get.getRuntimeClasspath)
        setMain(mainClass)
        setArgsString(outputDirectory.toString)
        super.exec()
      }
    }
  }
}
