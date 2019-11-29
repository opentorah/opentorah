package org.podval.fop

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.net.URI

import org.apache.fop.apps.{FOUserAgent, FopFactory}
import org.apache.fop.fonts.{FontEventListener, FontTriplet}
import org.apache.fop.tools.fontlist.{FontListGenerator, FontSpec}
import org.apache.xmlgraphics.util.MimeConstants
import org.podval.fop.util.Util.mapValues
import org.podval.fop.util.{Logger, Util}
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
    saxon: Saxon,
    configurationFile: File,
    creationDate: Option[String],
    author: Option[String],
    title: Option[String],
    subject: Option[String],
    keywords: Option[String],
    inputFile: File,
    inputDirectory: File, // TODO calculate from the input file
    outputFile: File,
    plugin: Option[FopPlugin],
    logger: Logger
  ): Unit = {
    logger.debug(
      s"""Fop.run(
         |  configurationFile = $configurationFile,
         |  inputFile = $inputFile,
         |  inputDirectory = $inputDirectory,
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
  def listFonts(configurationFile: File): Unit = {
    val fontFamilies: SortedMap[String, List[FontSpec]] = getFontFamilies(configurationFile)

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

    System.out.print(result.toString)
    System.out.flush()
  }

  def getFontFiles(configurationFile: File, fontFamilyNames: List[String], logger: Logger): String =
    if (fontFamilyNames.isEmpty) "" else {
      val fontFamilyNamesStr: String = fontFamilyNames.mkString(", ")
      val fontFamilies: Map[String, List[FontSpec]] = getFontFamilies(configurationFile)
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

      val result: String = files.map(uri => new File(uri.getPath).getAbsolutePath).mkString(", ")
      logger.info(s"Fop.getFontFiles($fontFamilyNamesStr) = $result.")
      result
    }

  private def getFontFamilies(configurationFile: File): SortedMap[String, List[FontSpec]] = {
    val fopFactory: FopFactory = FopFactoryFactory.newFactory(configurationFile)

    val fontEventListener: FontEventListener  = new FontEventListener {
      override def fontLoadingErrorAtAutoDetection(source: AnyRef, fontURL: String, e: Exception): Unit =
        System.err.println(s"Could not load $fontURL (${e.getLocalizedMessage})")
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
