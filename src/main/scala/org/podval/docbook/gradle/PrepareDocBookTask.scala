package org.podval.docbook.gradle

import org.gradle.api.artifacts.Configuration
import org.gradle.api.{Action, DefaultTask}
import org.gradle.api.file.{CopySpec, FileCopyDetails, RelativePath}
import org.gradle.api.provider.MapProperty
import org.gradle.api.tasks.{Input, TaskAction}
import java.io.{File, InputStream}
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
    //    copyCustomizations(layout.forXslt2, Set("html", "fo"))

    // XSLT stylesheets
    unpackDocBookXsl(layout.forXslt1, "XSLT")
    //    unpackDocBookXsl(layout.forXslt2, "XSLT 2.0")

    // substitutions DTD
    Util.writeInto(layout.substitutionsDtdFile, substitutions.get.asScala.toSeq.map {
      case (name: String, value: String) => s"""<!ENTITY $name "$value">"""
    }.mkString("", "\n", "\n"))

    writeCatalog("xml", "catalog.xml", layout.catalogFile)
  }

  private def copyCustomizations(forXslt: ForXslt, customizations: Set[String]): Unit =
    customizations.foreach { name: String =>
      copyResource(forXslt.stylesheetDirectoryName, name + ".xsl", forXslt.stylesheetFile(name))
    }

  private def writeCatalog(directory: String, name: String, file: File): Unit = {
    if (file.exists()) {
      logger.info(s"Skipping catalog: file $file already exists")
    } else {
      logger.info(s"Writing catalog to $file")
      file.getParentFile.mkdirs

      val is: InputStream = getClass.getResourceAsStream(s"/$directory/$name")
      if (is == null) {
        val message: String = s"Catalog not found"
        logger.error(message)
        throw new IllegalArgumentException(message)
      }
      val template: String = Util.readFrom(is)
      val catalog: String = template
        .replace("@docBookXsl@", layout.forXslt1.docBookXslDirectoryRelative)
        .replace("@docBookXsl2@", layout.forXslt2.docBookXslDirectoryRelative)
        .replace("@data@", layout.dataDirectoryRelative)
        .replace("@substitutions-dtd@", layout.substitutionsDtdRelative)
      Util.writeInto(file, catalog)
    }
  }

  private def copyResource(directory: String, name: String, file: File): Unit = {
    if (file.exists()) {
      logger.info(s"Skipping configuration resource $name: file $file already exists")
    } else {
      logger.info(s"Writing $file")
      file.getParentFile.mkdirs

      val is: InputStream = getClass.getResourceAsStream(s"/$directory/$name")
      if (is == null) {
        val message: String = s"Configuration resource not found: $name"
        logger.error(message)
        throw new IllegalArgumentException(message)
      }
      java.nio.file.Files.copy(is, file.toPath)
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
