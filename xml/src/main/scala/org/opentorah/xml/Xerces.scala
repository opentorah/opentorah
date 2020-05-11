package org.opentorah.xml

import javax.xml.parsers.SAXParserFactory
import org.w3c.dom.Node
import org.xml.sax.{XMLFilter, XMLReader}

object Xerces {

  def toString(node: Node): String = xmlSerializer.writeToString(node)

  private val xmlSerializer: org.apache.xml.serializer.dom3.LSSerializerImpl = {
    val result = new org.apache.xml.serializer.dom3.LSSerializerImpl
    result.setParameter("format-pretty-print", true)
    result
  }

  val saxParserFactoryName: String = classOf[org.apache.xerces.jaxp.SAXParserFactoryImpl].getName
  val saxParserName: String = classOf[org.apache.xerces.parsers.SAXParser].getName

  private def newSaxParserFactory: SAXParserFactory = {
    val result = new org.apache.xerces.jaxp.SAXParserFactoryImpl
    result.setXIncludeAware(true)
    result
  }

  def getFilteredXMLReader(filters: Seq[XMLFilter]): XMLReader =
    filters.foldLeft(getXMLReader) { case (parent, filter) =>
        filter.setParent(parent)
        filter
    }

  private def getXMLReader: XMLReader =
    newSaxParserFactory.newSAXParser.getXMLReader
}
