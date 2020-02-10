package org.digitaljudaica.xml

import cats.implicits._
import java.io.{File, FileInputStream, FileNotFoundException}
import java.net.URL
import org.digitaljudaica.metadata.{HasName, Names, WithName}
import org.digitaljudaica.util.Collections
import org.xml.sax.InputSource
import scala.xml.{Elem, Utility, XML}

object Load {

  type Error = Exception

  type Result = (String, Either[Error, Elem])

  def fromResource(resource: Resource): Result = {
    val clazz = resource.obj.getClass
    val name = resource.nameEffective + ".xml"

    Option(clazz.getResource(name)).fold[Result](
      (s"Resource $clazz/$name", Left(new FileNotFoundException()))
    )(fromUrl)
  }

  def fromResourceDo(resource: Resource): Elem =
    doLoad(fromResource(resource))

  def fromUrl(url: URL): Result =
    fromInputSource(new InputSource(url.openStream()), url.toString)

  def fromFile(directory: File, fileName: String): Result =
    fromFile(new File(directory, fileName + ".xml"))

  def fromFile(file: File): Result =
    fromInputSource(new InputSource(new FileInputStream(file)), s"file $file")

  def fromFileDo(directory: File, fileName: String): Elem =
    doLoad(fromFile(directory, fileName))

  // This is lazy to allow correct initialization: the code uses values(),
  // Language metadata file references Language instances by name :)
  def names[K <: WithName](
     keys: Seq[K],
     resource: Resource
  ): Map[K, Names] = bind(
    keys,
    loadMetadata(resource, wrapped(rootElementName = "names", typeName = resource.nameEffective, elementName = "names", Names.parser)))

  def metadataUsingNames[K <: WithName, M](
    keys: Seq[K],
    resource: Resource,
    elementName: String,
    parser: Parse.Parser[M]
  ): Map[K, M] = {
    val metadatas = metadata(resource, elementName, Names.withNames(parser))
    val result = findAndBind(keys, metadatas, (metadata: (Names, M), name: String) => metadata._1.hasName(name)).toMap
    Collections.mapValues(result)(_._2)
  }

  def metadata[M](
    resource: Resource,
    elementName: String,
    parser: Parse.Parser[M]
  ): Seq[M] = loadMetadata(resource, wrapped("metadata", resource.nameEffective, elementName, parser))

  def wrapped[A](rootElementName: String, typeName: String, elementName: String, parser: Parse.Parser[A]): Parse.Parser[Seq[A]] =
    Parse.checkName(rootElementName, for {
      type_ <- Parse.requiredAttribute("type")
      _ <- Parse.check(type_ == typeName, s"Wrong metadata type: $type_ instead of $typeName")
      result <- Parse.elements(elementName, parser)
    } yield result)

  def loadMetadata[A](resource: Resource, parser: Parse.Parser[A]): A =
    Parse.runA(Parse.parse(Load.fromResource(resource), parser))

  def bind[K <: WithName, M <: HasName](keys: Seq[K], metadatas: Seq[M]): Map[K, M] =
    findAndBind(keys, metadatas, (metadata: M, name: String) => metadata.hasName(name)).toMap

  private def findAndBind[K <: WithName, M](keys: Seq[K], metadatas: Seq[M], hasName: (M, String) => Boolean): Seq[(K, M)] = {
    if (keys.isEmpty) require(metadatas.isEmpty, s"Unmatched metadatas: ${metadatas.mkString("\n")}")
    if (metadatas.isEmpty) require(keys.isEmpty, s"Unmatched keys: $keys")
    Collections.checkNoDuplicates(keys, s"keys")

    if (keys.isEmpty) Nil else {
      val key: K = keys.head
      val (withName: Seq[M], withoutName: Seq[M]) = metadatas.partition(metadata => hasName(metadata, key.name))
      require(withName.nonEmpty, s"No metadata for ${key.name}")
      require(withName.length == 1)
      (key, withName.head) +: findAndBind(keys.tail, withoutName, hasName)
    }
  }

  private def fromInputSource(source: InputSource, publicId: String): Result = {
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
