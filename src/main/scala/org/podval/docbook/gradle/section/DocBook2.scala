package org.podval.docbook.gradle.section

import java.io.File

import org.podval.docbook.gradle.{Layout, Logger}

trait DocBook2 extends Section {

  def usesIntermediate: Boolean = false

  def intermediateDirectoryName: String = name

  def outputFileExtension: String

  def intermediateFileExtension: String = outputFileExtension

  // From general to specific
  final def parameterSections: List[Section] =  {
    val crossProcessorSections: List[DocBook2] = additionalSections.flatMap { _ match {
        case docBook2: DocBook2 => Some(docBook2)
        case _ => None
      }
    }
    if (crossProcessorSections.nonEmpty)
      throw new IllegalArgumentException(s"Cross-processor sections not supported: $crossProcessorSections")

    if (additionalSections.contains(Common) && additionalSections.contains(HtmlCommon) &&
        (additionalSections != List(HtmlCommon, Common)))
      throw new IllegalArgumentException(s"Wrong section order for $this: $additionalSections")

    (this +: additionalSections).reverse
  }

  def additionalSections: List[Section]

  def stylesheetUriName: String

  def usesRootFile: Boolean

  def parameter(parameter: DocBook2 => Option[String], value: String): Option[(String, String)] =
    parameter(this).map(_ -> value)

  def baseDirParameter: Option[String] = None

  def rootFilenameParameter: Option[String] = None

  final def usesCss: Boolean = htmlStylesheetsParameter.nonEmpty

  def htmlStylesheetsParameter: Option[String] = None

  def epubEmbeddedFontsParameter: Option[String] = None

  def chunkQuietlyParameter: Option[String] = None

  final def rootFilename(inputFileName: String): String =
    outputFileNameOverride.getOrElse(inputFileName)

  def outputFileNameOverride: Option[String] = None

  def copyDestinationDirectoryName: Option[String] = None

  def postProcess(
    layout: Layout,
    substitutions: Map[String, String],
    isJEuclidEnabled: Boolean,
    inputDirectory: File,
    inputFile: File,
    outputFile: File,
    logger: Logger
  ): Unit = {
  }
}

object DocBook2 {

  val all: List[DocBook2] = List(Html, Epub2, Epub3, Pdf, Html2)

  def forName(name: String): DocBook2 = {
    all.find(_.name.equalsIgnoreCase(name)).getOrElse {
      throw new IllegalArgumentException(
        s"""Unsupported output format $name;
           |supported formats are: ${getNames(all)}
           |""".stripMargin
      )
    }
  }

  def getNames(processors: List[DocBook2]): String =
    "[" + processors.map(docBook2 => "\"" + docBook2.name +"\"").mkString(", ") + "]"
}
