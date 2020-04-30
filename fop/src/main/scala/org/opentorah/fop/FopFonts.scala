package org.opentorah.fop

import java.io.File
import java.net.URI
import org.apache.fop.apps.FopFactory
import org.apache.fop.fonts.{FontEventListener, FontTriplet}
import org.apache.fop.tools.fontlist.{FontListGenerator, FontSpec}
import org.apache.xmlgraphics.util.MimeConstants
import org.opentorah.util.Collections.mapValues
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable.SortedMap
// TODO for Scala 2.13: import scala.jdk.CollectionConverters._
import scala.collection.JavaConverters._

object FopFonts {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  // Inspired by org.apache.fop.tools.fontlist.FontListMain:
  def list(configurationFile: File): String = {
    val fontFamilies: SortedMap[String, List[FontSpec]] = getFamilies(configurationFile)

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

  def getFiles(configurationFile: File, fontFamilyNames: List[String]): List[URI] =
    if (fontFamilyNames.isEmpty) List.empty else {
      val fontFamilies: Map[String, List[FontSpec]] = getFamilies(configurationFile)
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

  private def getFamilies(configurationFile: File): SortedMap[String, List[FontSpec]] = {
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

  def deleteCache(configurationFile: File): Unit =
    FopFactoryFactory.newFactory(configurationFile).newFOUserAgent.getFontManager.deleteCache()
}
