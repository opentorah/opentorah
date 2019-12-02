package org.podval.fop

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.net.URI

import org.apache.fop.apps.{FOUserAgent, FopFactory}
import org.apache.fop.fonts.{FontEventListener, FontTriplet}
import org.apache.fop.tools.fontlist.{FontListGenerator, FontSpec}
import org.apache.xmlgraphics.util.MimeConstants
import org.podval.fop.util.Util.mapValues
import org.podval.fop.util.{Logger, TestLogger, Util}
import org.podval.fop.xml.{Saxon, Xml}

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters._

object Fop {

  val defaultConfigurationFile: String =
    s"""${Xml.header}
       |<fop version="1.0">
       |  <renderers>
       |    <renderer mime="application/pdf">
       |      <fonts>
       |        <!-- FOP will detect fonts available in the operating system. -->
       |        <auto-detect/>
       |      </fonts>
       |    </renderer>
       |  </renderers>
       |</fop>
       |"""

  private val dateFormat: java.text.DateFormat = new java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")

  def run(
    saxon: Saxon = Saxon.Saxon6,
    configurationFile: File,
    creationDate: Option[String] = None,
    author: Option[String] = None,
    title: Option[String] = None,
    subject: Option[String] = None,
    keywords: Option[String] = None,
    inputFile: File,
    outputFile: File,
    // TODO update
    // To use installed NodeJS:
    //   plugin = Some(MathJaxFopPlugin.get(nodeModulesParent, logger))
    // To use JEuclid:
    //   plugin = Some(new JEuclidFopPlugin)
    plugin: Option[FopPlugin] = None,
    logger: Logger = new TestLogger
  ): Unit = {
    logger.debug(
      s"""Fop.run(
         |  configurationFile = $configurationFile,
         |  inputFile = $inputFile,
         |  outputFile = $outputFile,
         |)""".stripMargin
    )

    val fopFactory: FopFactory = FopFactoryFactory.newFactory(configurationFile, inputFile)

    plugin.foreach(_.configure(fopFactory))

    // PDF metadata:
    val foUserAgent: FOUserAgent = fopFactory.newFOUserAgent

    setPdfMetadata(
      foUserAgent,
      creationDate,
      author,
      title,
      subject,
      keywords
    )

    run(
      saxon,
      fopFactory,
      foUserAgent,
      inputFile,
      outputFile,
      logger
    )
  }

  def setPdfMetadata(
    foUserAgent: FOUserAgent,
    creationDate: Option[String],
    author: Option[String],
    title: Option[String],
    subject: Option[String],
    keywords: Option[String]
  ): Unit = {
    foUserAgent.setCreator(Util.applicationString)
    creationDate.foreach { creationDate =>
      val value: java.util.Date = dateFormat.parse(creationDate)
      foUserAgent.setCreationDate(value)
    }

    foUserAgent.setAuthor(author.orNull)
    foUserAgent.setTitle(title.orNull)
    foUserAgent.setSubject(subject.orNull)
    foUserAgent.setKeywords(keywords.orNull)
  }

  def run(
    saxon: Saxon,
    fopFactory: FopFactory,
    foUserAgent: FOUserAgent,
    inputFile: File,
    outputFile: File,
    logger: Logger
  ): Unit = {
    val outputStream: OutputStream = new BufferedOutputStream(new FileOutputStream(outputFile))
    val fop: org.apache.fop.apps.Fop = fopFactory.newFop("application/pdf", foUserAgent, outputStream)

    try {
      saxon.transform(
        inputFile = inputFile,
        defaultHandler = fop.getDefaultHandler,
        logger = logger
      )
    } finally {
      outputStream.close()
    }
  }

  // Inspired by org.apache.fop.tools.fontlist.FontListMain:
  def listFonts(configurationFile: File, logger: Logger): String = {
    val fontFamilies: SortedMap[String, List[FontSpec]] = getFontFamilies(configurationFile, logger)

    val result: StringBuilder = new StringBuilder

    for ((firstFamilyName: String, fontSpecs: List[FontSpec]) <- fontFamilies) {
      result.append(s"$firstFamilyName:\n")
      for (fontSpec: FontSpec <- fontSpecs) {
        val uriStr: String = Option(fontSpec.getFontMetrics.getFontURI).map(_.toString).getOrElse("---")
        result.append(s"  ${fontSpec.getKey} ${fontSpec.getFamilyNames} ($uriStr)\n")

        val triplets: Seq[FontTriplet] = fontSpec.getTriplets
          .asInstanceOf[java.util.Collection[FontTriplet]]
          .asScala.toSeq

        for (triplet: FontTriplet <- triplets) result.append(s"    $triplet\n")
      }
    }

    result.toString
  }

  def getFontFiles(configurationFile: File, fontFamilyNames: List[String], logger: Logger): List[URI] =
    if (fontFamilyNames.isEmpty) List.empty else {
      val fontFamilies: Map[String, List[FontSpec]] = getFontFamilies(configurationFile, logger)
      val uris: List[URI] = fontFamilyNames.flatMap { fontFamilyName: String =>
        fontFamilies.get(fontFamilyName).fold[List[URI]] {
          logger.error(s"Font family $fontFamilyName not found!")
          List.empty
        } { fontSpecs: List[FontSpec] => fontSpecs.flatMap { fontSpec: FontSpec =>
          val uri: Option[URI] = Option(fontSpec.getFontMetrics.getFontURI)
          if (uri.isEmpty) logger.error(s"No URI for fontSpec ${fontSpec.getKey}!")
          uri
        }}
      }

      val (files: List[URI], nonFiles: List[URI]) = uris.partition(_.getScheme == "file")
      if (nonFiles.nonEmpty) logger.error(s"Non-file URIs: $nonFiles")

      files
    }

  private def getFontFamilies(configurationFile: File, logger: Logger): SortedMap[String, List[FontSpec]] = {
    val fopFactory: FopFactory = FopFactoryFactory.newFactory(configurationFile)

    val fontEventListener: FontEventListener  = new FontEventListener {
      override def fontLoadingErrorAtAutoDetection(source: AnyRef, fontURL: String, e: Exception): Unit =
        logger.error(s"Could not load $fontURL (${e.getLocalizedMessage})")
      override def fontSubstituted(source: AnyRef, requested: FontTriplet, effective: FontTriplet): Unit = {}
      override def glyphNotAvailable(source: AnyRef, ch: Char, fontName: String): Unit = {}
      override def fontDirectoryNotFound(source: AnyRef, msg: String): Unit = {}
      override def svgTextStrokedAsShapes(source: AnyRef, fontFamily: String): Unit = {}
    }

    SortedMap[String, List[FontSpec]]() ++ mapValues(new FontListGenerator()
      .listFonts(fopFactory, MimeConstants.MIME_PDF, fontEventListener)
      .asInstanceOf[java.util.SortedMap[String, java.util.List[FontSpec]]]
      .asScala.toMap)(_.asScala.toList)
  }

  def deleteFontCache(configurationFile: File): Unit =
    FopFactoryFactory.newFactory(configurationFile).newFOUserAgent.getFontManager.deleteCache()
}
