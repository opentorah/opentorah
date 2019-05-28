package org.podval.docbook.gradle

import org.gradle.api.provider.Property
import org.gradle.api.plugins.JavaPluginConvention
import org.gradle.api.tasks.{Input, JavaExec, SourceSet}
import java.io.File
import scala.beans.BeanProperty

class DocBookDataTask extends JavaExec {
  @Input @BeanProperty val dataGeneratorClass: Property[String] =
    getProject.getObjects.property(classOf[String])

  private val dataDirectory: File = Layout.forProject(getProject).dataDirectory

  getOutputs.dir(dataDirectory)

  private val logger: Logger = new Logger.PluginLogger(getProject.getLogger)

  override def exec(): Unit = {
    val mainClass: String = dataGeneratorClass.get
    if (mainClass.isEmpty) {
      logger.info("Skipping DocBook data generation: dataGenerationClass is not set")
    } else {
      val mainSourceSet: Option[SourceSet] =
        Option(getProject.getConvention.findPlugin(classOf[JavaPluginConvention]))
          .map(_.getSourceSets.getByName(SourceSet.MAIN_SOURCE_SET_NAME))

      if (mainSourceSet.isEmpty) {
        logger.info("Skipping DocBook data generation: no Java plugin in the project")
      } else {
        logger.info(s"Running DocBook data generator $mainClass into $dataDirectory")
        setClasspath(mainSourceSet.get.getRuntimeClasspath)
        setMain(mainClass)
        setArgsString(dataDirectory.toString)
        super.exec()
      }
    }
  }
}
