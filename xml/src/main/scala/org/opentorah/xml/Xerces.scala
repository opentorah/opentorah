package org.opentorah.xml

import javax.xml.parsers.{SAXParser, SAXParserFactory}
import org.xml.sax.{XMLFilter, XMLReader}

object Xerces {

  val saxParserFactoryProperty: String = classOf[javax.xml.parsers.SAXParserFactory].getName
  val saxParserFactoryName: String = classOf[org.apache.xerces.jaxp.SAXParserFactoryImpl].getName
  val saxParserName: String = classOf[org.apache.xerces.parsers.SAXParser].getName

  private def newSaxParserFactory: SAXParserFactory = {
    val result = new org.apache.xerces.jaxp.SAXParserFactoryImpl
    result.setXIncludeAware(true)
    // result.setNamespaceAware(true)
    // result.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
    result
  }

  def getParser: SAXParser = newSaxParserFactory.newSAXParser

  def getXMLReader: XMLReader = getParser.getXMLReader

  def getFilteredXMLReader(filters: Seq[XMLFilter]): XMLReader =
    filters.foldLeft(getXMLReader) { case (parent, filter) =>
      filter.setParent(parent)
      filter
    }
}
