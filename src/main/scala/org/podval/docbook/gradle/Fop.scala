package org.podval.docbook.gradle

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.net.URI

import net.sourceforge.jeuclid.fop.plugin.JEuclidFopFactoryConfigurator
import org.apache.fop.apps.{FOUserAgent, FopConfParser, FopFactory}
import org.apache.xmlgraphics.util.MimeConstants
import org.apache.fop.fonts.{FontEventListener, FontTriplet}
import org.apache.fop.tools.fontlist.{FontListGenerator, FontSpec}
import org.podval.docbook.gradle.mathjax.{MathJaxFopPlugin, MathJax}

import scala.collection.JavaConverters._
import scala.collection.immutable.SortedMap

object Fop {

  def run(
    configurationFile: File,
    nodeModulesRoot: File,
    substitutions: Map[String, String],
    isMathJaxEnabled: Boolean,
    isJEuclidEnabled: Boolean,
    inputFile: File,
    inputDirectory: File,
    outputFile: File,
    logger: Logger
  ): Unit = {
    logger.info(
      s"""Fop.run(
         |  configurationFile = $configurationFile,
         |  nodeModulesRoot = $nodeModulesRoot,
         |  inputFile = $inputFile,
         |  inputDirectory = $inputDirectory,
         |  outputFile = $outputFile,
         |  isMathJaxEnabled = $isMathJaxEnabled,
         |  isJEuclidEnabled = $isJEuclidEnabled
         |)""".stripMargin
    )

    val fopFactory: FopFactory = getFopFactory(configurationFile, inputFile)

    require(!isMathJaxEnabled || !isJEuclidEnabled)
    if (isMathJaxEnabled) MathJaxFopPlugin.configure(fopFactory, nodeModulesRoot, MathJax.Config())
    else if (isJEuclidEnabled) JEuclidFopFactoryConfigurator.configure(fopFactory)

    // PDF metadata:
    val foUserAgent: FOUserAgent = fopFactory.newFOUserAgent

    foUserAgent.setCreator(Util.applicationString)
    substitutions.get("creationDate").foreach { creationDate =>
      val format: java.text.DateFormat = new java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")
      val value: java.util.Date = format.parse(creationDate)
      foUserAgent.setCreationDate(value)
    }

    def set(name: String, setter: FOUserAgent => String => Unit): Unit =
      setter(foUserAgent)(substitutions.get(name).orNull)
    set("author", _.setAuthor)
    set("title", _.setTitle)
    set("subject", _.setSubject)
    set("keywords", _.setKeywords)

    val outputStream: OutputStream = new BufferedOutputStream(new FileOutputStream(outputFile))
    val fop: org.apache.fop.apps.Fop = fopFactory.newFop("application/pdf", foUserAgent, outputStream)

    try {
      Xml.transform(
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
        val uri: Option[URI] = Option(fontSpec.getFontMetrics.getFontURI)
        val triplets: Seq[FontTriplet] = fontSpec.getTriplets
          .asInstanceOf[java.util.Collection[FontTriplet]]
          .asScala.toSeq

        val uriStr: String = uri.map(_.toString).getOrElse("---")
        result.append(s"  ${fontSpec.getKey} ${fontSpec.getFamilyNames} ($uriStr)\n")

        for (triplet: FontTriplet <- triplets)
          result.append(s"    $triplet\n")
      }
    }

    System.out.print(result.toString)
    System.out.flush()
  }

  def getFontFiles(configurationFile: File, fontFamilyNames: List[String], logger: Logger): String = {
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

  def getFontFamilies(configurationFile: File): SortedMap[String, List[FontSpec]] = {
    val fopFactory: FopFactory = getFopFactory(configurationFile, configurationFile)

    val fontEventListener: FontEventListener  = new FontEventListener {
      override def fontLoadingErrorAtAutoDetection(source: AnyRef, fontURL: String, e: Exception): Unit =
        System.err.println(s"Could not load $fontURL (${e.getLocalizedMessage})")
      override def fontSubstituted(source: AnyRef, requested: FontTriplet, effective: FontTriplet): Unit = {}
      override def glyphNotAvailable(source: AnyRef, ch: Char, fontName: String): Unit = {}
      override def fontDirectoryNotFound(source: AnyRef, msg: String): Unit = {}
      override def svgTextStrokedAsShapes(source: AnyRef, fontFamily: String): Unit = {}
    }

    SortedMap[String, List[FontSpec]]() ++ new FontListGenerator()
      .listFonts(fopFactory, MimeConstants.MIME_PDF, fontEventListener)
      .asInstanceOf[java.util.SortedMap[String, java.util.List[FontSpec]]]
      .asScala.toMap
      .mapValues(_.asScala.toList)
  }

  private def getFopFactory(configurationFile: File, inputFile: File): FopFactory =
    new FopConfParser(configurationFile, inputFile.getParentFile.toURI)
      .getFopFactoryBuilder
      .build
}
