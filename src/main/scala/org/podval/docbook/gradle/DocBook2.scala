package org.podval.docbook.gradle

import java.io.File

abstract class DocBook2 {

  def name: String

  def stylesheetName: String = name

  def usesIntermediate: Boolean

  def outputDirectoryName: String = name

  def intermediateDirectoryName: String = outputDirectoryName

  def outputFileExtension: String

  def intermediateFileExtension: String = outputFileExtension

  def parameterSections: Set[String] =
    Set(name, stylesheetName) ++
      (if (usesDocBookXslt2) Set.empty else Set("common") ++ (if (!usesHtml) Set.empty else Set("htmlCommon")))

  def usesDocBookXslt2: Boolean

  def stylesheetUriName: String

  def usesHtml: Boolean

  def usesCss: Boolean

  def isEpub: Boolean = false

  def outputFileNameOverride: Option[String] = None

  def copyDestinationDirectoryName: Option[String] = None

  def postProcess(
    layout: Layout,
    isJEuclidEnabled: Boolean,
    inputDirectory: File,
    inputFile: File,
    outputFile: File,
    logger: Logger
  ): Unit = {
  }
}

object DocBook2 {

  trait HtmlLike extends DocBook2 {
    final override def usesHtml: Boolean = true
    final override def usesCss: Boolean = true
    final override def usesIntermediate: Boolean = false
    final override def outputFileExtension: String = "html"
    final override def outputFileNameOverride: Option[String] = Some("index")
  }

  object Html extends DocBook2 with HtmlLike {
    override def usesDocBookXslt2: Boolean = false
    override def name: String = "html"
    override def stylesheetUriName: String = "html/chunkfast"
  }

  object Html2 extends DocBook2 with HtmlLike {
    override def usesDocBookXslt2: Boolean = true
    override def name: String = "html2"
    override def stylesheetUriName: String = "html/chunk"
  }

  object Pdf extends DocBook2 {
    override def usesDocBookXslt2: Boolean = false
    override def usesHtml: Boolean = false
    override def usesCss: Boolean = false

    override def usesIntermediate: Boolean = true
    override def intermediateDirectoryName: String = "fo"
    override def intermediateFileExtension: String = "fo"

    override def name: String = "pdf"
    override def outputFileExtension: String = "pdf"

    override def stylesheetUriName: String = "fo/docbook"

    override def postProcess(
      layout: Layout,
      isJEuclidEnabled: Boolean,
      inputDirectory: File,
      inputFile: File,
      outputFile: File,
      logger: Logger
    ): Unit = Fop.run(
      configurationFile = layout.fopConfigurationFile,
      isJEuclidEnabled = isJEuclidEnabled,
      inputFile = inputFile,
      inputDirectory = inputDirectory,
      outputFile = outputFile,
      logger = logger
    )
  }

  trait Epub extends DocBook2 {
    final override def usesDocBookXslt2: Boolean = false
    final override def usesHtml: Boolean = true
    final override def usesCss: Boolean = false
    final override def isEpub: Boolean = true
    final override def usesIntermediate: Boolean = true
    final override def outputFileExtension: String = "epub"
    final override def copyDestinationDirectoryName: Option[String] = Some("OEBPS")

    final override def postProcess(
      layout: Layout,
      isJEuclidEnabled: Boolean,
      inputDirectory: File,
      inputFile: File,
      outputFile: File,
      logger: Logger
    ): Unit = {
      val zip = new org.apache.tools.ant.taskdefs.Zip
      zip.setProject(new org.apache.tools.ant.Project)
      zip.setPreserve0Permissions(true)
      zip.setCompress(false)
      zip.setDestFile(outputFile)
      val fileSet = new org.apache.tools.ant.types.FileSet()
      fileSet.setDir(inputDirectory)
      fileSet.appendIncludes(Array("mimetype", "META-INF/**", "OEBPS/**"))
      zip.addFileset(fileSet)
      zip.execute()
    }
  }

  object Epub2 extends Epub {
    override def name: String = "epub2"
    override def stylesheetUriName: String = "epub/docbook"
    // DocBook XSLT stylesheets for EPUB2 do not add the mimetype file with "application/epub+zip" in it!
  }

  object Epub3 extends Epub {
    override def name: String = "epub3"
    override def stylesheetUriName: String = "epub3/chunk"
    // DocBook XSLT stylesheets for EPUB3 use chunkfast (for xhtml) internally already :)
  }

  val processors: List[DocBook2] = List(Html, Epub2, Epub3, Pdf, Html2)
}
