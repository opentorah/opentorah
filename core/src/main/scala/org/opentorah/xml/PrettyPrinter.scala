package org.opentorah.xml

import org.opentorah.util.{Files, Strings}
import org.typelevel.paiges.Doc
import java.io.File

final class PrettyPrinter(
  val width: Int = 120,
  val indent: Int = 2,
  val encodeXmlSpecials: Boolean = true,
  val doNotStackElements: Set[String] = Set.empty,
  val alwaysStackElements: Set[String] = Set.empty,
  val nestElements: Set[String] = Set.empty,
  val clingyElements: Set[String] = Set.empty,
  val allowEmptyElements: Boolean = true,
  val keepEmptyElements: Set[String] = Set.empty,
  val preformattedElements: Set[String] = Set.empty
):
  def copy(
    width: Int = width,
    indent: Int = indent,
    encodeXmlSpecials: Boolean = encodeXmlSpecials,
    doNotStackElements: Set[String] = doNotStackElements,
    alwaysStackElements: Set[String] = alwaysStackElements,
    nestElements: Set[String] = nestElements,
    clingyElements: Set[String] = clingyElements,
    allowEmptyElements: Boolean = allowEmptyElements,
    keepEmptyElements: Set[String] = keepEmptyElements,
    preformattedElements: Set[String] = preformattedElements
  ): PrettyPrinter = PrettyPrinter(
    width,
    indent,
    encodeXmlSpecials,
    doNotStackElements,
    alwaysStackElements,
    nestElements,
    clingyElements,
    allowEmptyElements,
    keepEmptyElements,
    preformattedElements
  )

  @scala.annotation.targetName("add")
  def +(other: PrettyPrinter): PrettyPrinter = copy(
    doNotStackElements = doNotStackElements ++ other.doNotStackElements,
    alwaysStackElements = alwaysStackElements ++ other.alwaysStackElements,
    nestElements = nestElements ++ other.nestElements,
    clingyElements = clingyElements ++ other.clingyElements,
    keepEmptyElements = keepEmptyElements ++ other.keepEmptyElements,
    preformattedElements = preformattedElements ++ other.preformattedElements
  )

  def render(
    doctype: Option[String] = None,
    element: Xml.Element
  ): String = render(addXmlHeader = false, doctype = doctype, element)

  def renderWithHeader(
    doctype: Option[String] = None,
    element: Xml.Element
  ): String =  render(addXmlHeader = true, doctype = doctype, element)

  private def render(
    addXmlHeader: Boolean,
    doctype: Option[String],
    element: Xml.Element
  ): String =
    val prefix: String =
      (if !addXmlHeader then "" else Xml.header + "\n") +
      doctype.fold("")(doctype => doctype + "\n")

    val result: String = fromElement(
      element,
      parent = None,
      canBreakLeft = true,
      canBreakRight = true
    )
      .render(width)
      .replace(PrettyPrinter.hiddenNewline, "\n")

    prefix + result + "\n"

  def write(
    file: File,
    replace: Boolean = true,
    doctype: Option[String] = None,
    element: Xml.Element
  ): Unit = Files.write(
    file,
    replace,
    content = renderWithHeader(doctype, element)
  )

  private def fromPreformattedElement(element: Xml.Element, parent: Option[Xml.Element]): Seq[String] =
    val attributeValues: Attribute.StringValues = getAttributeValues(element, parent)
    val attributes: String = if attributeValues.isEmpty then "" else attributeValues
      .map(attributeValue =>  attributeValue.attribute.qName + "=\"" + attributeValue.valueEffective.get + "\"")
      .mkString(" ", ", ", "")

    val children: Seq[String] =
      Xml.getChildren(element).flatMap(node => fromPreformattedNode(node, Some(element)))

    val name: String = getName(element)

    if children.isEmpty then Seq(s"<$name$attributes/>")
    else if children.length == 1 then Seq(s"<$name$attributes>" + children.head + s"</$name>")
    else Seq(s"<$name$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$name>")

  private def fromElement(
    element: Xml.Element,
    parent: Option[Xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    val attributeValues: Attribute.StringValues = getAttributeValues(element, parent)

    val attributes: Doc =
      if attributeValues.isEmpty then Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, attributeValues.map(attributeValue =>
        Doc.text(attributeValue.attribute.qName + "=") + Doc.lineOrEmpty +
        // Note: maybe use single quotes if the value contains double quote?
        Doc.text("\"" + encodeXmlSpecials(attributeValue.valueEffective.get) + "\"")
      ))

    val nodes: Xml.Nodes = atomize(Seq.empty, Xml.getChildren(element))
    val whitespaceLeft: Boolean = nodes.headOption.exists(Xml.isWhitespace)
    val whitespaceRight: Boolean = nodes.lastOption.exists(Xml.isWhitespace)
    val charactersLeft: Boolean = nodes.headOption.exists(Xml.isCharacters)
    val charactersRight: Boolean = nodes.lastOption.exists(Xml.isCharacters)
    val chunks: Seq[Xml.Nodes] = chunkify(Seq.empty, Seq.empty, nodes, flush = false)
    val noText: Boolean = chunks.forall(_.forall(!Xml.isText(_)))

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

    val label: String = Xml.getName(element)
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

  private def getName(element: Xml.Element): String =
    Xml.getPrefix(element).fold("")(_ + ":") + Xml.getName(element)

  private def getAttributeValues(element: Xml.Element, parent: Option[Xml.Element]): Attribute.StringValues =
    val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(Namespace.getAll)
    Namespace.getAll(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
    Attribute.get(element).filterNot(_.value.isEmpty)

  @scala.annotation.tailrec
  private def atomize(result: Xml.Nodes, nodes: Xml.Nodes): Xml.Nodes =
    if nodes.isEmpty then result else
      val (texts: Xml.Nodes, tail: Xml.Nodes) = nodes.span(Xml.isText)

      val newResult: Xml.Nodes = if texts.isEmpty then result else result ++
        processText(Seq.empty,
          Strings.squashBigWhitespace(texts.map(Xml.asText).map(Xml.getText).mkString("")))

      if tail.isEmpty then newResult else atomize(newResult :+ tail.head, tail.tail)

  @scala.annotation.tailrec
  private def processText(result: Seq[Xml.Text], text: String): Seq[Xml.Text] =
    if text.isEmpty then result else
      val (spaces: String, tail: String) = text.span(_ == ' ')
      val newResult = if spaces.isEmpty then result else result :+ Xml.mkText(" ")
      val (word: String, tail2: String) = tail.span(_ != ' ')

      if word.isEmpty then newResult else processText(newResult :+ Xml.mkText(word), tail2)

  @scala.annotation.tailrec
  private def chunkify(
    result: Seq[Xml.Nodes],
    current: Xml.Nodes,
    nodes: Xml.Nodes,
    flush: Boolean
  ): Seq[Xml.Nodes] =
    if flush then chunkify(result :+ current.reverse, Nil, nodes, flush = false) else (current, nodes) match
      case (Nil    , Nil    )                                         => result
      case (_      , Nil    )                                         => chunkify(result, current     , Nil     , flush = true )
      case (Nil    , n :: ns) if  Xml.isWhitespace(n)                 => chunkify(result, Nil         , ns      , flush = false)
      case (_      , n :: ns) if  Xml.isWhitespace(n)                 => chunkify(result, current     , ns      , flush = true )
      case (Nil    , n :: ns) if !Xml.isWhitespace(n)                 => chunkify(result, n :: Nil    , ns      , flush = false)
      case (c :: cs, n :: ns) if !Xml.isWhitespace(n) &&  cling(c, n) => chunkify(result, n :: c :: cs, ns      , flush = false)
      case (c :: _ , n :: ns) if !Xml.isWhitespace(n) && !cling(c, n) => chunkify(result, current     , n :: ns , flush = true )

  private def cling(c: Xml.Node, n: Xml.Node): Boolean = Xml.isText(c) || Xml.isText(n) ||
    (Xml.isElement(n) && clingyElements.contains(Xml.getName(Xml.asElement(n))))

  private def fromChunk(
    nodes: Xml.Nodes,
    parent: Option[Xml.Element],
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

  private def fromPreformattedNode(node: Xml.Node, parent: Option[Xml.Element]): Seq[String] =
    if Xml.isElement(node) then fromPreformattedElement(Xml.asElement(node), parent)
    else if Xml.isText(node) then preformattedLines(Xml.getText(Xml.asText(node)))
    else preformattedLines(Xml.toString(node))

  private def fromNode(
    node: Xml.Node,
    parent: Option[Xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    if Xml.isElement(node) then
      val element: Xml.Element = Xml.asElement(node)
      if preformattedElements.contains(Xml.getName(element)) then
        Doc.text(fromPreformattedElement(element, parent).mkString(PrettyPrinter.hiddenNewline))
      else
        val result: Doc = fromElement(element, parent, canBreakLeft, canBreakRight)
        // Note: suppressing extra hardLine when lb is in stack is non-trivial - and not worth it :)
        if canBreakRight && Xml.getName(element) == "lb" then result + Doc.hardLine else result
    else if Xml.isText(node) then Doc.text(encodeXmlSpecials(Xml.getText(Xml.asText(node))))
    else Doc.paragraph(Xml.toString(node))

  private def preformattedLines(string: String): Seq[String] =
    encodeXmlSpecials(string).split("\n").toSeq

  // Since I now encode XML specials when pretty-printing, there is no need for the elaborate double-escaping
  // in the Markdown code-blocks, so DoubleEscapeHtmlRendererExtension was removed.
  private def encodeXmlSpecials(string: String): String =
    if encodeXmlSpecials then Strings.encodeXmlSpecials(string) else string

object PrettyPrinter:

  // The only way I found to not let Paiges screw up indentation in the <pre><code>..</code></pre> blocks
  // is to give it the whole block as one unbreakable text, and for that I need to hide newlines from it -
  // and then restore them in render()...
  // Also, element start and end tags must not be separated from the children by newlines...
  private val hiddenNewline: String = "\\n"

  val default: PrettyPrinter = new PrettyPrinter
