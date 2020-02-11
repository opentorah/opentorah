package org.digitaljudaica.xml

import cats.implicits._
import org.digitaljudaica.metadata.{HasName, Names, WithName}
import org.digitaljudaica.util.Collections

// TODO rename Parse!
object Load {

  // TODO do I really need to pass the `from` through in the result? Or is it going to always be at hand anyway?
  def apply[A](from: From, parser: Parser[A]): ErrorOr[A] = Context.run(
    from.load match {
      case (from, what) => what match {
        case Left(exception) => lift(Left(exception.toString))
        case Right(elem) => Context.complete(Context.nested(Some(from), elem, parser))
      }
    }
  )

  // This is lazy to allow correct initialization: the code uses values(),
  // Language metadata file references Language instances by name :)
  def names[K <: WithName](
     keys: Seq[K],
     from: From.FromResource // TODO generalize to just From...
  ): Map[K, Names] = bind(
    keys,
    metadata(from, wrapped(rootElementName = "names", typeName = from.nameEffective, elementName = "names", Names.parser)))

  def metadataUsingNames[K <: WithName, M](
    keys: Seq[K],
    from: From.FromResource, // TODO generalize to just From...
    elementName: String,
    parser: Parser[M]
  ): Map[K, M] = {
    val metadatas = metadata(from, elementName, Names.withNames(parser))
    val result = findAndBind(keys, metadatas, (metadata: (Names, M), name: String) => metadata._1.hasName(name)).toMap
    Collections.mapValues(result)(_._2)
  }

  def metadata[M](
    from: From.FromResource, // TODO generalize to just From...
    elementName: String,
    parser: Parser[M]
  ): Seq[M] = metadata(from, wrapped("metadata", from.nameEffective, elementName, parser))

  // TODO move out of here?
  def wrapped[A](rootElementName: String, typeName: String, elementName: String, parser: Parser[A]): Parser[Seq[A]] =
    Element.checkName(rootElementName, for {
      type_ <- Attribute.required("type")
      _ <- Check(type_ == typeName, s"Wrong metadata type: $type_ instead of $typeName")
      result <- Element.all(elementName, parser)
    } yield result)

  def metadata[A](from: From, parser: Parser[A]): A =
    runA(Load(from, parser))

  def runA[A](result: ErrorOr[A]): A =
    result.fold(error => throw new IllegalArgumentException(error), identity)

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
