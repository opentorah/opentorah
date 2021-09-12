package org.opentorah.xml

import org.xml.sax.helpers.XMLFilterImpl
import org.xml.sax.{Attributes, Locator}

final class TracingFilter extends XMLFilterImpl:

  override def setDocumentLocator(locator: Locator): Unit =
    println(s"setDocumentLocator($locator)")
    super.setDocumentLocator(locator)

  override def startDocument(): Unit =
    println(s"startDocument")
    super.startDocument()

  override def endDocument(): Unit =
    println(s"endDocument")
    super.endDocument()

  override def startPrefixMapping(prefix: String, uri: String): Unit =
    println(s"""startPrefixMapping(xmlns${if prefix.isEmpty then "" else ":" + prefix}="$uri")""")
    super.startPrefixMapping(prefix, uri)

  override def endPrefixMapping(prefix: String): Unit =
    println(s"endPrefixMapping($prefix)")
    super.endPrefixMapping(prefix)

  override def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit =
    println(s"""startElement(uri="$uri", localName="$localName", qName="$qName" """)
    for i <- 0 until atts.getLength do
      println(s"""  attribute: uri="${atts.getURI(i)}", localName="${atts.getLocalName(i)}", qName="${atts.getQName(i)}", type=${atts.getType(i)}, value="${atts.getValue(i)}" """)
    println(")")
    super.startElement(uri, localName, qName, atts)

  override def endElement(uri: String, localName: String, qName: String): Unit =
    println(s"""endElement(uri="$uri", localName="$localName", qName="$qName")""")
    super.endElement(uri, localName, qName)

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    val chars = ch.slice(start, start + length).mkString("")
    println(s"""characters("$chars")""")
    super.characters(ch, start, length)

  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit =
    val chars = ch.slice(start, start + length).mkString("")
    println(s"ignorableWhitespace($chars)")
    super.ignorableWhitespace(ch, start, length)

  override def processingInstruction(target: String, data: String): Unit =
    println(s"processingInstruction($target, $data)")
    super.processingInstruction(target, data)

  override def skippedEntity(name: String): Unit =
    println(s"skippedEntity($name)")
    super.skippedEntity(name)
