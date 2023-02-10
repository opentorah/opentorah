package org.opentorah.xml

import org.slf4j.Logger
import org.xml.sax.{ErrorHandler, SAXParseException, XMLFilter, XMLReader}

object Xerces:

  val saxParserFactoryProperty: String = classOf[javax.xml.parsers.SAXParserFactory].getName
  val saxParserFactoryName: String = classOf[org.apache.xerces.jaxp.SAXParserFactoryImpl].getName
  val saxParserName: String = classOf[org.apache.xerces.parsers.SAXParser].getName

  enum ProcessIncludes derives CanEqual:
    case No
    case YesWithBases
    case YesWithoutBases

  def getXMLReader(
    filters: Seq[XMLFilter],
    resolver: Option[Resolver],
    processIncludes: ProcessIncludes,
    logger: Logger
  ): XMLReader =
    val saxParserFactory: javax.xml.parsers.SAXParserFactory = org.apache.xerces.jaxp.SAXParserFactoryImpl()

    processIncludes match
      case ProcessIncludes.YesWithBases | ProcessIncludes.YesWithoutBases =>
        saxParserFactory.setNamespaceAware(true) // needed for XIncludeAware to kick in
        saxParserFactory.setXIncludeAware(true)
      case _ =>

    processIncludes match
      case ProcessIncludes.YesWithoutBases =>
        saxParserFactory.setFeature(
          org.apache.xerces.impl.Constants.XERCES_FEATURE_PREFIX + org.apache.xerces.impl.Constants.XINCLUDE_FIXUP_BASE_URIS_FEATURE,
          false
        )
      case _ =>

    val result: XMLReader = filters.foldLeft(saxParserFactory.newSAXParser.getXMLReader)((parent, filter) =>
      filter.setParent(parent)
      filter
    )

    resolver.foreach(resolver => result.setEntityResolver(resolver))
    result.setErrorHandler(errorHandler(logger))

    result

  private def errorHandler(logger: Logger): ErrorHandler = new ErrorHandler:
    override def warning   (exception: SAXParseException): Unit = logger.warn (message(exception))
    override def error     (exception: SAXParseException): Unit = logger.error(message(exception))
    override def fatalError(exception: SAXParseException): Unit = logger.error(message(exception))

    private def message(exception: SAXParseException): String =
      s"${exception.getMessage} in ${exception.getSystemId}(${exception.getPublicId}) at ${exception.getLineNumber}:${exception.getColumnNumber}"
