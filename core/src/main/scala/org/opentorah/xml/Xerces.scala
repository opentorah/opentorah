package org.opentorah.xml

import org.opentorah.util.{Effects, Files}
import org.slf4j.{Logger, LoggerFactory}
import org.xml.sax.{ErrorHandler, SAXParseException, XMLReader}
import zio.ZIO

object Xerces:

  enum ProcessIncludes derives CanEqual:
    case No
    case YesWithBases
    case YesWithoutBases

  def getXMLReader(processIncludes: ProcessIncludes = Xerces.ProcessIncludes.YesWithBases): XMLReader =
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

    val result: XMLReader = saxParserFactory.newSAXParser.getXMLReader
    result.setErrorHandler(errorHandler(logger))

    result

  private def errorHandler(logger: Logger): ErrorHandler = new ErrorHandler:
    override def warning   (exception: SAXParseException): Unit = logger.warn (message(exception))
    override def error     (exception: SAXParseException): Unit = logger.error(message(exception))
    override def fatalError(exception: SAXParseException): Unit = logger.error(message(exception))

    private def message(exception: SAXParseException): String =
      s"${exception.getMessage} in ${exception.getSystemId}(${exception.getPublicId}) at ${exception.getLineNumber}:${exception.getColumnNumber}"

  val logger: Logger = LoggerFactory.getLogger("org.opentorah.xml") // TODO eliminate

  // This is mind-bogglingly weird, but:
  // - Xerces has a bug in the handling of XIncludes;
  // - starting at the third level of nested includes, the values of the xml:base attributes are wrong;
  // - the bug https://issues.apache.org/jira/browse/XERCESJ-1102 was reported in October 2005!!!
  // - a patch that allegedly fixes the issue is known for years
  // - a comment from the Xerces maintainer says:
  //   What Xerces needs most is new contributors / committers who can volunteer their time and help review these patches and get them committed.
  //   We also need a new release. It's been 5 years. Long overdue.
  //   If you or anyone else is interested in getting involved we'd be happy to have you join the project.
  // - latest release of Xerces was in 2023, with the bug still there
  // - many projects depend on Xerces, including Saxon, where the bug was also discussed: https://saxonica.plan.io/issues/4664
  // - allegedly, the bug is fixed in "SaxonC 11.1" - although how can this be with Saxon not shipping its own Xerces is not clear.
  //
  // So, I need to process XIncludes myself instead of relying on the industry-standard Xerces!
  // What a nightmare...
  def fixXIncludeBug(element: Xml.Element): ZIO[Any, Effects.Error, Xml.Element] =
    def fix(current: Option[String], level: Int, element: Xml.Element): ZIO[Any, Effects.Error, Xml.Element] = for
      base: Option[String] <- Xml.baseAttribute.optional.get(element)

      (baseFixed: Option[String], levelNew: Int) = base match
        case None => (current, level)
        case Some(base) =>
          val baseFixed: String = current match
            case None => base
            case Some(current) =>
              val basePath: Seq[String] = Files.splitUrl(base)
              val missing: Seq[String] = Files.splitUrl(current)
                .init // drop the xml file at the end
                .takeWhile(_ != basePath.head)
              if missing.isEmpty then base else (missing ++ basePath).mkString("/")
          (Some(baseFixed), level + 1)

      childrenFixed: Xml.Nodes <- ZIO.collectAll(
        for child: Xml.Node <- Xml.getChildren(element)
          yield if !Xml.isElement(child) then ZIO.succeed(child) else fix(baseFixed, levelNew, Xml.asElement(child))
      )
    yield
      val result: Xml.Element = Xml.setChildren(element, childrenFixed)
      if base.isEmpty then result else
        Xml.baseAttribute.required.withValue(baseFixed.get).add(Xml.baseAttribute.remove(result))

    fix(current = None, level = 0, element = element)

