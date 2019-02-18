package org.podval.docbook.gradle

import org.gradle.api.artifacts.Configuration
import org.gradle.api.{Action, DefaultTask}
import org.gradle.api.file.{CopySpec, FileCopyDetails, RelativePath}
import org.gradle.api.provider.MapProperty
import org.gradle.api.tasks.{Input, TaskAction}
import java.io.File
import scala.beans.BeanProperty
import scala.collection.JavaConverters._

class PrepareDocBookTask extends DefaultTask  {

  private val layout: Layout = Layout.forProject(getProject)

  private val logger: Logger = new Logger.PluginLogger(getLogger)

  @Input @BeanProperty val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @TaskAction
  def prepareDocBook(): Unit = {
    // Common configuration files
    copyResource("css", "docBook.css", layout.cssFile)
    copyResource("fop", "fop.xconf", layout.fopConfigurationFile)

    // Customizations
    copyCustomizations(layout.forXslt1, Set("common", "common-html", "epub",  "epub3", "fo", "html"))
    copyCustomizations(layout.forXslt2, Set("html"))

    // XSLT stylesheets
    unpackDocBookXsl(layout.forXslt1, "XSLT")
    unpackDocBookXsl(layout.forXslt2, "XSLT 2.0")

    // substitutions DTD
    Util.writeInto(layout.substitutionsDtdFile, substitutions.get.asScala.toSeq.map {
      case (name: String, value: String) => s"""<!ENTITY $name "$value">"""
    }.mkString("", "\n", "\n"))

    writeCatalog(layout.catalogFile)
  }

  private def copyCustomizations(forXslt: ForXslt, customizations: Set[String]): Unit =
    customizations.foreach { name: String =>
      copyResource(forXslt.stylesheetDirectoryName, name + ".xsl", forXslt.stylesheetFile(name))
    }

  private def writeCatalog(file: File): Unit = writeFile(file){
    Util.readFrom(getClass,  "/xml/catalog.xml")
      .replace("@docBookXsl@", layout.forXslt1.docBookXslDirectoryRelative)
      .replace("@docBookXsl2@", layout.forXslt2.docBookXslDirectoryRelative)
      .replace("@data@", layout.dataDirectoryRelative)
      .replace("@substitutions-dtd@", layout.substitutionsDtdRelative)
  }

  private def copyResource(directory: String, name: String, file: File): Unit =
    writeFile(file)(Util.readFrom(getClass, s"/$directory/$name"))

  private def writeFile(toFile: File)(content: => String): Unit = {
    if (toFile.exists()) {
      logger.info(s"Skipping: file $toFile already exists")
    } else {
      logger.info(s"Writing $toFile")
      toFile.getParentFile.mkdirs

      Util.writeInto(toFile, content)
    }
  }

  private def unpackDocBookXsl(layout: ForXslt, name: String): Unit = {
    val directory: File = layout.docBookXslDirectory
    if (!directory.exists) {
      logger.info(s"Preparing DocBook $name stylesheets")

      val configuration: Configuration = getProject.getConfigurations.findByName(layout.docBookXslConfigurationName)
      val zip: File = configuration.getSingleFile
      val toDrop: Int = layout.docBookXslArchiveSubdirectoryName.count(_ == '/') + 1
      getProject.copy(new Action[CopySpec] {
        override def execute(copySpec: CopySpec): Unit = copySpec
          .into(directory)
          .from(getProject.zipTree(zip))
          // following 7 lines of code deal with extracting just the "docbook" directory;
          // this should become easier in Gradle 5.3, see:
          // https://github.com/gradle/gradle/issues/1108
          // https://github.com/gradle/gradle/pull/5405
          .include(layout.docBookXslArchiveSubdirectoryName +  "/**")
          .eachFile(new Action[FileCopyDetails] {
            override def execute(file: FileCopyDetails): Unit = {
              file.setRelativePath(new RelativePath(true, file.getRelativePath.getSegments.drop(toDrop): _*))
            }
          })
          .setIncludeEmptyDirs(false)
      })
    }
  }
}
