package org.opentorah.xml

import org.opentorah.util.{Files, Strings}
import org.typelevel.paiges.Doc
import java.io.File

final case class PrettyPrinter(
  width: Int = 120,
  indent: Int = 2,
  encodeXmlSpecials: Boolean = true,
  doNotStackElements: Set[String] = Set.empty,
  alwaysStackElements: Set[String] = Set.empty,
  nestElements: Set[String] = Set.empty,
  clingyElements: Set[String] = Set.empty,
  allowEmptyElements: Boolean = true,
  keepEmptyElements: Set[String] = Set.empty,
  preformattedElements: Set[String] = Set.empty
):

  def render(
    xml: Xml,
    doctype: Option[Doctype] = None
  )(
    element: xml.Element
  ): String = render(xml, addXmlHeader = false, doctype = doctype)(element)

  def renderWithHeader(
    xml: Xml,
    doctype: Option[Doctype] = None
  )(
    element: xml.Element
  ): String =  render(xml, addXmlHeader = true, doctype = doctype)(element)

  private def render(
    xml: Xml,
    addXmlHeader: Boolean,
    doctype: Option[Doctype]
  )(
    element: xml.Element
  ): String =
    val prefix: String =
      (if !addXmlHeader then "" else Xml.header + "\n") +
      doctype.fold("")(doctype => doctype.doctype + "\n")

    val forModel: ForModel = ForModel(xml)

    val result: String = forModel.fromElement(
      element.asInstanceOf[forModel.xml.Element], // TODO: yuck...
      parent = None,
      canBreakLeft = true,
      canBreakRight = true
    )
      .render(width)
      .replace(PrettyPrinter.hiddenNewline, "\n")

    prefix + result + "\n"

  def write(
    file: File,
    replace: Boolean,
    doctype: Option[Doctype] = None,
    element: ScalaXml.Element
  ): Unit = Files.write(
    file,
    replace,
    content = renderWithHeader(ScalaXml, doctype)(element)
  )

  // Note: methods below all need access to xml: Xml, often - for the types of parameters, so parameter 'xml'
  // must come first, and thus can not be implicit; it is cleaner to scope them in a class with 'xml' a constructor parameter.
  private final class ForModel(val xml: Xml):

    private def fromPreformattedElement(element: xml.Element, parent: Option[xml.Element]): Seq[String] =
      val attributeValues: Seq[Attribute.Value[String]] = getAttributeValues(element, parent)
      val attributes: String = if attributeValues.isEmpty then "" else attributeValues
        .map(attributeValue =>  attributeValue.attribute.qName + "=\"" + attributeValue.valueToString.get + "\"")
        .mkString(" ", ", ", "")

      val children: Seq[String] =
        xml.getChildren(element).flatMap(node => fromPreformattedNode(node, Some(element)))

      val name: String = getName(element)

      if children.isEmpty then Seq(s"<$name$attributes/>")
      else if children.length == 1 then Seq(s"<$name$attributes>" + children.head + s"</$name>")
      else Seq(s"<$name$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$name>")

    def fromElement(
      element: xml.Element,
      parent: Option[xml.Element],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc =
      val attributeValues: Seq[Attribute.Value[String]] = getAttributeValues(element, parent)

      val attributes: Doc =
        if attributeValues.isEmpty then Doc.empty
        else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, attributeValues.map(attributeValue =>
          Doc.text(attributeValue.attribute.qName + "=") + Doc.lineOrEmpty +
          // Note: maybe use single quotes if the value contains double quote?
          Doc.text("\"" + encodeXmlSpecials(attributeValue.valueToString.get) + "\"")
        ))

      val nodes: xml.Nodes = atomize(Seq.empty, xml.getChildren(element))
      val whitespaceLeft: Boolean = nodes.headOption.exists(xml.isWhitespace)
      val whitespaceRight: Boolean = nodes.lastOption.exists(xml.isWhitespace)
      val charactersLeft: Boolean = nodes.headOption.exists(xml.isCharacters)
      val charactersRight: Boolean = nodes.lastOption.exists(xml.isCharacters)
      val chunks: Seq[xml.Nodes] = chunkify(Seq.empty, Seq.empty, nodes, flush = false)
      val noText: Boolean = chunks.forall(_.forall(!xml.isText(_)))

      val children: Seq[Doc] =
        val canBreakLeft1 = canBreakLeft || whitespaceLeft
        val canBreakRight1 = canBreakRight || whitespaceRight

        if chunks.isEmpty then Seq.empty
        else if chunks.length == 1 then Seq(
          fromChunk(chunks.head, Some(element), canBreakLeft1, canBreakRight1)
        ) else
          fromChunk(chunks.head, Some(element), canBreakLeft = canBreakLeft1, canBreakRight = true) +:
          chunks.tail.init.map(chunk => fromChunk(chunk, Some(element), canBreakLeft = true, canBreakRight = true)) :+
          fromChunk(chunks.last, Some(element), canBreakLeft = true, canBreakRight = canBreakRight1)

      val label: String = xml.getName(element)
      val name: String = getName(element)

      if children.isEmpty then
        Doc.text(s"<$name") + attributes + Doc.lineOrEmpty +
          (if allowEmptyElements || keepEmptyElements.contains(name) then Doc.text("/>") else Doc.text(s"></$name>"))
      else
        val start: Doc = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">")
        val end: Doc = Doc.text(s"</$name>")

        val stackElements: Boolean = noText &&
          ((children.length >= 2) || ((children.length == 1) && alwaysStackElements.contains(label))) &&
          !doNotStackElements.contains(label)

        if stackElements then
          // If this is clearly a bunch of elements - stack 'em with an indent:
          Doc.cat(Seq(
            start,
            Doc.cat(children.map(child => (Doc.hardLine + child).nested(indent))),
            Doc.hardLine,
            end
          ))
        else if nestElements.contains(label) then
          // If this is forced-nested element - nest it:
          Doc.intercalate(Doc.lineOrSpace, children).tightBracketBy(left = start, right = end, indent)
        else
          // Mixed content or non-break-off-able attachments on the side(s) cause flow-style;
          // character content should stick to the opening and closing tags:
          Doc.cat(Seq(
            start,
            if canBreakLeft && !charactersLeft then Doc.lineOrEmpty else Doc.empty,
            Doc.intercalate(Doc.lineOrSpace, children),
            if canBreakRight && !charactersRight then Doc.lineOrEmpty else Doc.empty,
            end
          ))

    private def getName(element: xml.Element): String =
      xml.getPrefix(element).fold("")(_ + ":") + xml.getName(element)

    private def getAttributeValues(element: xml.Element, parent: Option[xml.Element]): Seq[Attribute.Value[String]] =
      val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(xml.getNamespaces)
      xml.getNamespaces(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
      xml.getAttributes(element).filterNot(_.value.isEmpty)

    @scala.annotation.tailrec
    private def atomize(result: xml.Nodes, nodes: xml.Nodes): xml.Nodes =
      if nodes.isEmpty then result else
        val (texts: xml.Nodes, tail: xml.Nodes) = nodes.span(xml.isText)

        val newResult: xml.Nodes = if texts.isEmpty then result else result ++
          processText(Seq.empty, texts.head,
            Strings.squashBigWhitespace(texts.map(xml.asText).map(xml.getText).mkString("")))

        if tail.isEmpty then newResult
        else atomize(newResult :+ tail.head, tail.tail)

    @scala.annotation.tailrec
    private def processText(result: Seq[xml.Text], seed: xml.Node, text: String): Seq[xml.Text] =
      if text.isEmpty then result else
        val (spaces: String, tail: String) = text.span(_ == ' ')
        val newResult = if spaces.isEmpty then result else result :+ xml.mkText(" ", seed)
        val (word: String, tail2: String) = tail.span(_ != ' ')

        if word.isEmpty then newResult
        else processText(newResult :+ xml.mkText(word, seed), seed, tail2)

    @scala.annotation.tailrec
    private def chunkify(
      result: Seq[xml.Nodes],
      current: xml.Nodes,
      nodes: xml.Nodes,
      flush: Boolean
    ): Seq[xml.Nodes] =
      if flush then chunkify(result :+ current.reverse, Nil, nodes, flush = false) else (current, nodes) match
        case (Nil    , Nil    )                                         => result
        case (_      , Nil    )                                         => chunkify(result, current     , Nil     , flush = true )
        case (Nil    , n :: ns) if  xml.isWhitespace(n)                 => chunkify(result, Nil         , ns      , flush = false)
        case (_      , n :: ns) if  xml.isWhitespace(n)                 => chunkify(result, current     , ns      , flush = true )
        case (Nil    , n :: ns) if !xml.isWhitespace(n)                 => chunkify(result, n :: Nil    , ns      , flush = false)
        case (c :: cs, n :: ns) if !xml.isWhitespace(n) &&  cling(c, n) => chunkify(result, n :: c :: cs, ns      , flush = false)
        case (c :: _ , n :: ns) if !xml.isWhitespace(n) && !cling(c, n) => chunkify(result, current     , n :: ns , flush = true )

    private def cling(c: xml.Node, n: xml.Node): Boolean = xml.isText(c) || xml.isText(n) ||
      (xml.isElement(n) && clingyElements.contains(xml.getName(xml.asElement(n))))

    private def fromChunk(
      nodes: xml.Nodes,
      parent: Option[xml.Element],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc =
      require(nodes.nonEmpty)
      if nodes.length == 1 then
        fromNode(nodes.head, parent, canBreakLeft, canBreakRight)
      else Doc.intercalate(Doc.empty,
        fromNode(nodes.head, parent, canBreakLeft, canBreakRight = false) +:
        nodes.tail.init.map(node => fromNode(node, parent, canBreakLeft = false, canBreakRight = false)) :+
        fromNode(nodes.last, parent, canBreakLeft = false, canBreakRight)
      )

    private def fromPreformattedNode(node: xml.Node, parent: Option[xml.Element]): Seq[String] =
      if xml.isElement(node) then fromPreformattedElement(xml.asElement(node), parent)
      else if xml.isText(node) then preformattedLines(xml.getText(xml.asText(node)))
      else preformattedLines(xml.toString(node))

    private def fromNode(
      node: xml.Node,
      parent: Option[xml.Element],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc =
      if xml.isElement(node) then
        val element: xml.Element = xml.asElement(node)
        if preformattedElements.contains(xml.getName(element)) then
          Doc.text(fromPreformattedElement(element, parent).mkString(PrettyPrinter.hiddenNewline))
        else
          val result: Doc = fromElement(element, parent, canBreakLeft, canBreakRight)
          // Note: suppressing extra hardLine when lb is in stack is non-trivial - and not worth it :)
          if canBreakRight && xml.getName(element) == "lb" then result + Doc.hardLine else result
      else if xml.isText(node) then Doc.text(encodeXmlSpecials(xml.getText(xml.asText(node))))
      else Doc.paragraph(xml.toString(node))

  private def preformattedLines(string: String): Seq[String] =
    encodeXmlSpecials(string).split("\n").toSeq

  // Since I now encode XML specials when pretty-printing, there is no need for the elaborate double-escaping
  // in the Markdown code-blocks, so DoubleEscapeHtmlRendererExtension was removed.
  private def encodeXmlSpecials(string: String): String =
    if encodeXmlSpecials then Strings.encodeXmlSpecials(string) else string

object PrettyPrinter:

  trait Recognizer:
    def recognize(xml: Xml)(element: xml.Element): (PrettyPrinter, Option[Doctype])

  // The only way I found to not let Paiges screw up indentation in the <pre><code>..</code></pre> blocks
  // is to give it the whole block as one unbreakable text, and for that I need to hide newlines from it -
  // and then restore them in render()...
  // Also, element start and end tags must not be separated from the children by newlines...
  val hiddenNewline: String = "\\n"

  val default: PrettyPrinter = new PrettyPrinter

  def prettyPrint(roots: List[File], xml: Xml, recognizer: Recognizer): Unit =
    val (directories: List[File], files: List[File]) = roots.partition(_.isDirectory)
    val xmlFiles: List[File] = files.filter(isXml) ++ listXmlFiles(List.empty, directories)
    xmlFiles.foreach(file =>
      // Note: Scala XML ignores XML comments when parsing a file
      // (see https://github.com/scala/scala-xml/issues/508);
      // using Dom instead; use Dom to keep them...
      val element: xml.Element = xml.loadFromUrl(Files.file2url(file))
      val (prettyPrinter, doctype) = recognizer.recognize(xml)(element)
      Files.write(
        file = file,
        content = prettyPrinter.render(xml, addXmlHeader = true, doctype)(element)
      )
    )

  @scala.annotation.tailrec
  def listXmlFiles(result: List[File], directoriesToList: List[File]): List[File] = directoriesToList match
    case Nil => result
    case current :: tail =>
      val (directories: List[File], files: List[File]) = current.listFiles.toList.partition(_.isDirectory)
      listXmlFiles(result ++ files.filter(isXml), tail ++ directories)

  private def isXml(file: File): Boolean = Files.nameAndExtension(file.getName)._2.contains("xml")
