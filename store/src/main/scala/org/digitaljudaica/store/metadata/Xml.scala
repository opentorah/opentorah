package org.digitaljudaica.store.metadata

import java.io.{File, FileWriter, OutputStream, OutputStreamWriter, PrintWriter, Writer}

import scala.collection.mutable.WeakHashMap
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml._

object Xml {

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
  //  XML.withSAXParser(getParser)

  // --- XML validation with XSD; how do I do RNG?
  // https://github.com/scala/scala-xml/wiki/XML-validation
  // https://github.com/EdgeCaseBerg/scala-xsd-validation/blob/master/src/main/scala/LoadXmlWithSchema.scala

  def load(directory: File, fileName: String): Elem = {
    val file: File = new File(directory, fileName + ".xml")
    try {
      scala.xml.Utility.trimProper(scala.xml.XML.loadFile(file)).asInstanceOf[Elem]
    } catch {
      case e: org.xml.sax.SAXParseException =>
        throw new IllegalArgumentException(s"In file $file:", e)
    }
  }


  def loadResource(clazz: Class[_], name: String, tag: String): Elem =
    open1(XML.load(clazz.getResourceAsStream(name + ".xml")), tag)

  def loadMetadata(file: File): Elem = load(file, "index")

//  def load(file: File, tag: String): Elem = open(load(file), tag)

  def load(file: File): Elem = {
    if (!cache.contains(file)) {
      cache.put(file, Utility.trimProper(XML.loadFile(file)).asInstanceOf[Elem])
    }
    cache(file)
  }
  private[this] val cache: WeakHashMap[File, Elem] = new WeakHashMap[File, Elem]
  // TODO Introduce "release" method?



  private def removeNamespace(node: Node): Node = node match {
    case e: Elem => e.copy(scope = TopScope, child = e.child.map(removeNamespace))
    case n => n
  }

  private def removeNamespace(element: Elem): Elem =
    element.copy(scope = TopScope, child = element.child.map(removeNamespace))

  def spacedText(node: Node): String = node match {
    case elem: Elem => (elem.child map (_.text)).mkString(" ")
    case node: Node => node.text
  }

  def rewriteElements(xml: Elem, elementRewriter: Elem => Elem): Elem = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case element: Elem => elementRewriter(element)
        case other => other
      }
    }

    new RuleTransformer(rule).transform(xml).head.asInstanceOf[Elem]
  }

  // --- Pretty printing:
  private val width = 120

  // TODO: it seems that there is abug in PrettyPrinter:
  private val prettyPrinter: PrettyPrinter = new PrettyPrinter(width, 2) {
    override protected def makeBox(ind: Int, s: String): Unit =
      if (cur + s.length <= width) { // fits in this line; LMD: changed > to <=...
        items ::= Box(ind, s)
        cur += s.length
      } else try cut(s, ind) foreach (items ::= _) // break it up
      catch { case _: BrokenException => makePara(ind, s) } // give up, para

  }

  // TODO PrettyPrinter breaks the line between e1 and e2 in <e1>...</e1><e2>...</e2>
  // and between e1 and text in: <e1>...<e1>text;
  // should I try fixing that?
  // or implement my own based on http://www.lihaoyi.com/post/CompactStreamingPrettyPrintingofHierarchicalData.html ?
  // or move to DOM and use org.apache.xml.serializer.dom3.LSSerializerImpl?
  // or move via DOM to ScalaTags (implementation "com.lihaoyi:scalatags_$scalaVersionMajor:$scalaTagsVersion")?
  // or use `spotless` with the Eclipse formatter?
  private val join: Set[String] = Set(".", ",", ";", ":", "\"", ")")

  def format(elem: Elem): String = {
    @scala.annotation.tailrec
    def merge(result: List[String], lines: List[String]): List[String] = lines match {
      case l1 :: l2 :: ls =>
        val l = l2.trim
        if (join.exists(l.startsWith))
          merge(result, (l1 ++ l) :: ls)
        else
          merge(result :+ l1, l2 :: ls)
      case l :: Nil => result :+ l
      case Nil => result
    }

    val result: String = prettyPrinter.format(elem)

    // pretty-printer splits punctuation from the preceding elements; merge them back :)
    merge(List.empty, result.split("\n").toList).mkString("\n")
  }

  def print(xml: Node, outStream: OutputStream): Unit = print(xml, new OutputStreamWriter(outStream))
  def print(xml: Node, outFile: File): Unit = print(xml, new FileWriter(outFile))

  def print(xml: Node, writer: Writer): Unit = {
    val out = new PrintWriter(writer)
    val pretty = prettyPrinter.format(xml)
    // TODO when outputting XML, include <xml> header?
    out.println(pretty)
    out.close()
  }


  private def open1(what: Elem, tag: String): Elem = what/*(0).asInstanceOf[Elem].*/check(tag)

  def open(element: Elem, name: String): (Attributes, Seq[Elem]) = {
    checkName(element, name)
    checkNoNonElements(element)
    (Attributes(element), getElements(element))
  }

  def openEmpty(element: Elem, name: String): Attributes = {
    checkName(element, name)
    checkNoElements(element)
    checkNoNonElements(element)
    Attributes(element)
  }

  def openText(element: Elem, name: String): (Attributes, Option[String]) = {
    checkName(element, name)
    checkNoElements(element)
    val text = element.text
    (Attributes(element), if (text.isEmpty) None else Some(text))
  }

  private def checkName(element: Elem, name: String): Unit =
    require(element.label == name, s"Wrong element: ${element.label} instead of $name")

  private def checkNoElements(element: Elem): Unit =
    require(getElements(element).isEmpty, "Nested elements present.")

  private def checkNoNonElements(element: Elem): Unit = {
    val nonElements = getNonElements(element)
    require(nonElements.isEmpty, s"Non-element children present on element $element: $nonElements")
  }

  private def getElements(element: Elem): Seq[Elem] =
    element.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

  private def getNonElements(element: Elem): Seq[Node] = {
    element.child.filterNot(_.isInstanceOf[Elem]).filter { node =>
      !node.isInstanceOf[Text] ||
        node.asInstanceOf[Text].text.replace('\n', ' ').trim.nonEmpty
    }
  }

  def take(elements: Seq[Elem], name1: String): (Seq[Elem], Seq[Elem]) = {
    elements.span(_.label == name1)
  }

  def parseEmpty[T](element: Elem, name: String, parser: Attributes => T): T = {
    val attributes = openEmpty(element, name)
    val result = parser(attributes)
    attributes.close()
    result
  }

  def noMoreThanOne(elements: Seq[Elem]): Option[Elem] = {
    require(elements.length <= 1)
    elements.headOption
  }

  def span(elements: Seq[Elem], name1: String): Seq[Elem] = {
    val (result, tail) = take(elements, name1)
    checkNoMoreElements(tail)
    result
  }

  def span(elements: Seq[Elem], name1: String, name2: String): (Seq[Elem], Seq[Elem]) = {
    val (elements1, tail1) = elements.span(_.label == name1)
    val (elements2, tail2) = tail1.span(_.label == name2)
    checkNoMoreElements(tail2)
    (elements1, elements2)
  }

  def span(elements: Seq[Elem], name1: String, name2: String, name3: String): (Seq[Elem], Seq[Elem], Seq[Elem]) = {
    val (elements1, tail1) = elements.span(_.label == name1)
    val (elements2, tail2) = tail1.span(_.label == name2)
    val (elements3, tail3) = tail2.span(_.label == name3)
    checkNoMoreElements(tail3)
    (elements1, elements2, elements3)
  }

  def checkNoMoreElements(elements: Seq[Elem]): Unit =
    require(elements.isEmpty, s"Spurious elements: ${elements.head.label}")





  implicit class Ops(elem: Elem) {

    def elemsFilter(name: String): Seq[Elem] = elem.elems.filter(_.label == name)

    // TODO dup!
    def elems: Seq[Elem] = elem.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

    def elems(name: String): Seq[Elem] = {
      val result = elem.elems
      result.foreach(_.check(name))
      result
    }

    def elems(plural: String, singular: String, required: Boolean = true): Seq[Elem] =
      oneOptionalChild(plural, required).map(_.elems(singular)).getOrElse(Seq.empty)

    def descendants(name: String): Seq[Elem] = elem.flatMap(_ \\ name).filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

    def getAttribute(name: String): String = attributeOption(name).getOrElse(throw new NoSuchElementException(s"No attribute $name"))

    // TODO difference?
    def attributeOption(name: String): Option[String] = elem.attributes.asAttrMap.get(name)
//    def attributeOption(name: String): Option[String] = {
//      val result: Seq[Node] = elem \ ("@" + name)
//      if (result.isEmpty) None else Some(result.text)
//    }

    def idOption: Option[String] = attributeOption("xml:id")

    def id: String = getAttribute("xml:id")

    def intAttributeOption(name: String): Option[Int] = attributeOption(name).map { value =>
      try { value.toInt } catch { case e: NumberFormatException => throw new IllegalArgumentException(s"$value is not a number", e)}
    }

    def intAttribute(name: String): Int = intAttributeOption(name).getOrElse(throw new NoSuchElementException(s"No attribute $name"))

    def booleanAttribute(name: String): Boolean = {
      val value = attributeOption(name)
      value.isDefined && ((value.get == "true") || (value.get == "yes"))
    }

    def oneChild(name: String): Elem = oneOptionalChild(name, required = true).get
    def optionalChild(name: String): Option[Elem] = oneOptionalChild(name, required = false)

    private[this] def oneOptionalChild(name: String, required: Boolean = true): Option[Elem] = {
      val children = elem \ name

      if (children.size > 1) throw new NoSuchElementException(s"To many children with name '$name'")
      if (required && children.isEmpty) throw new NoSuchElementException(s"No child with name '$name'")

      if (children.isEmpty) None else Some(children.head.asInstanceOf[Elem])
    }

    def check(name: String): Elem = {
      if (elem.label != name) throw new NoSuchElementException(s"Expected name $name but got $elem.label")
      elem
    }

    def withoutNamespace: Elem = removeNamespace(elem)

    def format: String = Xml.format(elem)
  }
}
