package org.podval.judaica.parsers

import java.io.File

import org.podval.judaica.viewer.ViewerException

import scala.xml.Elem

class ParseException(file: File, cause: ViewerException) extends Exception(s"In file $file: ${cause.getMessage}", cause)

object ParseException {

  def withMetadataFile[T](file: File)(body: Elem => T): T = withFile(file)(body(XmlFile.loadMetadata(file)))


  private def withFile[T](file: File)(body: => T): T =
    try {
      body
    } catch {
      case e: ViewerException => throw new ParseException(file, e)
    }
}
