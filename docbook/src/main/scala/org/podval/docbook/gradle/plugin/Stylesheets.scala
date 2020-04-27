package org.podval.docbook.gradle.plugin

import java.io.File

import org.gradle.api.Project
import org.opentorah.fop.gradle.Gradle
import org.opentorah.fop.util.Logger

trait Stylesheets {
  protected def name: String

  protected def groupId: String
  protected def artifactId: String
  protected def classifier: Option[String]
  protected def extension: String

  def uri: String

  def directoryName: String

  protected def archiveSubdirectoryName: String

  def unpack(version: String, project: Project, layout: Layout, logger: Logger): Unit = {
    val directory: File = layout.docBookXslDirectory(directoryName)
    if (!directory.exists) {
      val classifierStr: String = classifier.fold("")(classifier => s":$classifier")
      val dependencyNotation: String = s"$groupId:$artifactId:$version$classifierStr@$extension"

      logger.info(s"Retrieving DocBook $name stylesheets: $dependencyNotation")
      val file: File = Gradle.getArtifact(project, dependencyNotation)

      logger.info(s"Unpacking ${file.getName}")
      Gradle.extract(
        project,
        zipFile = file,
        toExtract = archiveSubdirectoryName,
        isDirectory = true,
        into = directory
      )
    }
  }
}

object Stylesheets {
  object xslt1 extends Stylesheets {
    override def name: String = "XSLT"
    override def groupId: String = "net.sf.docbook"
    override def artifactId: String = "docbook-xsl"
    // classifier for the non-NS-aware stylesheets is "resources";
    // they  strip namespaces from DocBook V5.0 and produce a warning
    //   "namesp. cut : stripped namespace before processing";
    // during namespace stripping, the base URI of the document is lost;
    // see details in https://docbook.org/docs/howto/howto.html
    override def classifier: Option[String] = Some("ns-resources")
    override def extension: String = "zip"

    override def uri: String = "http://docbook.sourceforge.net/release/xsl-ns/current"
    override def directoryName: String = "docBookXsl"
    override def archiveSubdirectoryName: String = "docbook"
  }

  object xslt2 extends Stylesheets {
    override def name: String = "XSLT 2.0"
    override def groupId: String = "org.docbook"
    override def artifactId: String = "docbook-xslt2"
    override def classifier: Option[String] = None
    override def extension: String = "jar"

    override def uri: String = "https://cdn.docbook.org/release/latest/xslt"
    override def directoryName: String = "docBookXsl2"
    override def archiveSubdirectoryName: String = "xslt/base"
  }

  def apply(useXslt2: Boolean): Stylesheets = if (useXslt2) xslt2 else xslt1
}
