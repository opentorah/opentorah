package org.opentorah.docbook

import java.io.File

// TODO collapse into the XsltFormat
trait Format extends HasName derives CanEqual:
  final override def equals(other: Any): Boolean = other.isInstanceOf[Format] && this.name == other.asInstanceOf[Format].name

  // DirectFormat has those too, so that direct transform could try to interpret them

  // From general to specific
  def common: List[Common]

  def parameters: Map[String, String]

  def usesIntermediate: Boolean = false

  def outputFileExtension: String

  def intermediateFileExtension: String = outputFileExtension

  final def rootFilename(inputFileName: String): String = outputFileNameOverride.getOrElse(inputFileName)

  protected def outputFileNameOverride: Option[String] = None

  def postProcess(
    inputDirectory: File,
    outputFile: File
  ): Unit = {
  }

object Format:
  val all: List[Format] = List(Html, Epub2, Epub3, Pdf, Html2)

  def forName(name: String): Format = HasName.forName(name, all, "format")
