package org.digitaljudaica.xml

import java.io.{File, FileInputStream, FileNotFoundException}
import java.net.URL
import org.xml.sax.InputSource
import scala.xml.{Elem, Utility, XML}

object Load {

  type Error = Exception
  type Result = (String, Either[Error, Elem])

  def load(source: InputSource, publicId: String): Result = {
    val result = try {
      Right(Utility.trimProper(XML.load(source)).asInstanceOf[Elem])
    } catch {
      case e: FileNotFoundException => Left(e)
      case e: org.xml.sax.SAXParseException => Left(e)
    }
    (publicId, result)
  }

  def doLoad(result: Result): Elem = result._2 match {
    case Right(elem) => elem
    case Left(exception) =>
      throw new IllegalArgumentException(s"In ${result._1}", exception)
  }

  def fromResourceDo(obj: AnyRef, resourceName: String): Elem =
    doLoad(fromResource(obj, resourceName))

  def fromResource(obj: AnyRef, name: String): Result =
    fromResource(obj.getClass, name)

  def fromResource(clazz: Class[_], name: String): Result = {
    val resourceName = name + ".xml"
    Option(clazz.getResource(resourceName)).fold[Result](
      (s"Resource $clazz/$resourceName", Left(new FileNotFoundException()))
    )(fromUrl)
  }

  def fromUrl(url: URL): Result =
    load(new InputSource(url.openStream()), url.toString)

  def fromFile(file: File): Result =
    load(new InputSource(new FileInputStream(file)), s"file $file")

  def fromFileDo(directory: File, fileName: String): Elem =
    doLoad(fromFile(new File(directory, fileName + ".xml")))

  // --- Xerces parser with Scala XML:
  // build.gradle:    implementation "xerces:xercesImpl:$xercesVersion"
  //  def newSaxParserFactory: SAXParserFactory = {
  //    val result = SAXParserFactory.newInstance() // new org.apache.xerces.jaxp.SAXParserFactoryImpl
  //    result.setNamespaceAware(true)
  //    result.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
  //    //    result.setXIncludeAware(true)
  //    result
  //  }
  //
  //  def getParser: SAXParser = newSaxParserFactory.newSAXParser
  //  XML.withSAXParser(getParser) OR BETTER - XML.loadXML(InputSource, SAXParser)

  // --- XML validation with XSD; how do I do RNG?
  // https://github.com/scala/scala-xml/wiki/XML-validation
  // https://github.com/EdgeCaseBerg/scala-xsd-validation/blob/master/src/main/scala/LoadXmlWithSchema.scala
}
