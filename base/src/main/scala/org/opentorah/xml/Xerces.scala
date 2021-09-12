package org.opentorah.xml

import org.slf4j.Logger
import javax.xml.parsers.SAXParserFactory
import org.xml.sax.{ErrorHandler, SAXParseException, XMLFilter, XMLReader}

object Xerces:

  val saxParserFactoryProperty: String = classOf[javax.xml.parsers.SAXParserFactory].getName
  val saxParserFactoryName: String = classOf[org.apache.xerces.jaxp.SAXParserFactoryImpl].getName
  val saxParserName: String = classOf[org.apache.xerces.parsers.SAXParser].getName

  def getXMLReader(
    filters: Seq[XMLFilter],
    resolver: Option[Resolver],
    xincludeAware: Boolean
  ): XMLReader =
    val saxParserFactory: SAXParserFactory = org.apache.xerces.jaxp.SAXParserFactoryImpl()

    if xincludeAware then
      saxParserFactory.setNamespaceAware(true) // needed for XIncludeAware to kick in
      saxParserFactory.setXIncludeAware(true) // TODO cannot resolve symbol in IDE...
      // suppress adding `xml:base` on include:
      // constant `org.apache.xerces.parsers.XIncludeAwareParserConfiguration.XINCLUDE_FIXUP_BASE_URIS` is protected...
      saxParserFactory.setFeature("http://apache.org/xml/features/xinclude/fixup-base-uris", false)

    val result: XMLReader = filters.foldLeft(saxParserFactory.newSAXParser.getXMLReader)((parent, filter) =>
      filter.setParent(parent)
      filter
    )

    resolver.foreach(resolver => result.setEntityResolver(resolver))
    result.setErrorHandler(errorHandler(xmlLogger))

    result

  private def errorHandler(logger: Logger): ErrorHandler = new ErrorHandler:
    override def warning   (exception: SAXParseException): Unit = logger.warn (message(exception))
    override def error     (exception: SAXParseException): Unit = logger.error(message(exception))
    override def fatalError(exception: SAXParseException): Unit = logger.error(message(exception))

    private def message(exception: SAXParseException): String =
      s"${exception.getMessage} in ${exception.getSystemId}(${exception.getPublicId}) at ${exception.getLineNumber}:${exception.getColumnNumber}"
