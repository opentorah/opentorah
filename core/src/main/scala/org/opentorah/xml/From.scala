package org.opentorah.xml

import org.opentorah.platform.Platform
import org.opentorah.util.{Effects, Files}
import org.xml.sax.{ErrorHandler, InputSource, SAXParseException, XMLReader}
import org.slf4j.{Logger, LoggerFactory}
import scala.xml.XML
import zio.ZIO
import java.io.StringReader
import java.net.URL

sealed abstract class From(val name: String):
  def isInclude: Boolean

  def url: Option[URL]

  def load: Effects.IO[Element]


object From:
  enum ProcessIncludes derives CanEqual:
    case No
    case YesWithBases
    case YesWithoutBases
  
  private final class FromXml(
    name: String,
    element: Element
  ) extends From(name):
    override def isInclude: Boolean = false
    override def toString: String = s"From.xml($name)"
    override def url: Option[URL] = None
    // Note: if xml != this.xml, this will fail *at run-time*:
    override def load: Effects.IO[Element] = ZIO.succeed(element)

  def xml(name: String, elem: Element): From = FromXml(name, elem)

  private final class FromString(
    name: String,
    string: String
  ) extends From(name):
    override def isInclude: Boolean = false
    override def toString: String = s"From.string($name)"
    override def url: Option[URL] = None
    override def load: Effects.IO[Element] = read(
      processIncludes = ProcessIncludes.YesWithBases,
      inputSource = InputSource(StringReader(string)),
      fixXercesXIncludes = true
    )

  def string(name: String, string: String): From = FromString(name, string)

  private final class FromUrl(
    fromUrl: URL,
    override val isInclude: Boolean,
    processIncludes: ProcessIncludes
  ) extends From(Files.nameAndExtension(fromUrl.getPath)._1):
    override def toString: String = s"From.url($fromUrl, isInclude=$isInclude)"
    override def url: Option[URL] = Some(fromUrl)
    override def load: Effects.IO[Element] = read(
      processIncludes = processIncludes,
      inputSource = InputSource(fromUrl.toString),
      fixXercesXIncludes = true
    )

  def url(url: URL, processIncludes: ProcessIncludes = ProcessIncludes.YesWithBases): From = From.FromUrl(
    fromUrl = url,
    isInclude = false,
    processIncludes = processIncludes
  )
  private[xml] def include(url: URL): From = From.FromUrl(
    fromUrl = url,
    isInclude = true,
    processIncludes = ProcessIncludes.YesWithBases
  )

  private final class FromResource(
    clazz: Class[?],
    name: String,
    fixXercesXIncludes: Boolean // This is exposed just for tests
  ) extends From(name):
    override def isInclude: Boolean = false
    override def toString: String = s"From.resource($clazz:$name.xml)"
    override def url: Option[URL] = Option(clazz.getResource(name + ".xml"))
    override def load: Effects.IO[Element] = url
      .map(url => read(
        processIncludes = ProcessIncludes.YesWithBases,
        inputSource = InputSource(url.toString),
        fixXercesXIncludes = fixXercesXIncludes
      ))
      .getOrElse(Effects.fail(s"Resource not found: $this"))

  def resourceNamed(obj: AnyRef, name: String, fixXercesXIncludes: Boolean = true): From =
    FromResource(obj.getClass, name, fixXercesXIncludes)
  def resource(obj: AnyRef): From = resourceNamed(obj, Platform.className(obj))

  private def read(
    processIncludes: ProcessIncludes,
    inputSource: InputSource,
    fixXercesXIncludes: Boolean
  ): ZIO[Any, Effects.Error, Element] =
    val xmlReader: XMLReader = getXMLReader(processIncludes)
    for
      element: Element <- ZIO.succeed(XML.withXMLReader(xmlReader).load(inputSource))
      result <- if !fixXercesXIncludes then ZIO.succeed(element) else fixXIncludeBug(element)
    yield
      result

  private def getXMLReader(processIncludes: ProcessIncludes): XMLReader =
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
    override def warning(exception: SAXParseException): Unit = logger.warn(message(exception))
    override def error(exception: SAXParseException): Unit = logger.error(message(exception))
    override def fatalError(exception: SAXParseException): Unit = logger.error(message(exception))
    private def message(exception: SAXParseException): String =
      s"${exception.getMessage} in ${exception.getSystemId}(${exception.getPublicId}) at ${exception.getLineNumber}:${exception.getColumnNumber}"

  val logger: Logger = LoggerFactory.getLogger(classOf[From.type]) // TODO eliminate

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
  private def fixXIncludeBug(element: Element): ZIO[Any, Effects.Error, Element] =
    def fix(current: Option[String], level: Int, element: Element): ZIO[Any, Effects.Error, Element] = for
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

      childrenFixed: Nodes <- ZIO.collectAll(
        for child: Node <- Element.getChildren(element)
          yield if !Element.is(child) then ZIO.succeed(child) else fix(baseFixed, levelNew, Element.as(child))
      )
    yield
      val result: Element = Element.setChildren(element, childrenFixed)
      if base.isEmpty then result else Xml.baseAttribute.required.withValue(baseFixed.get).set(result)

    fix(current = None, level = 0, element = element)
