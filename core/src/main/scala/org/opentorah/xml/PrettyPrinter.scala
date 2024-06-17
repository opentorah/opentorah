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
    element: Element
  ): String = render(addXmlHeader = false, doctype = doctype, element)

  def renderWithHeader(
    doctype: Option[String] = None,
    element: Element
  ): String =  render(addXmlHeader = true, doctype = doctype, element)

  private def render(
    addXmlHeader: Boolean,
    doctype: Option[String],
    element: Element
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
    element: Element
  ): Unit = Files.write(
    file,
    replace,
    content = renderWithHeader(doctype, element)
  )

  private def fromPreformattedElement(element: Element, parent: Option[Element]): Seq[String] =
    val attributeValues: Attribute.StringValues = getAttributeValues(element, parent)
    val attributes: String = if attributeValues.isEmpty then "" else attributeValues
      .map(attributeValue =>  attributeValue.attribute.qName + "=\"" + attributeValue.valueEffective.get + "\"")
      .mkString(" ", ", ", "")

    val children: Seq[String] =
      Element.getChildren(element).flatMap(node => fromPreformattedNode(node, Some(element)))

    val name: String = getName(element)

    if children.isEmpty then Seq(s"<$name$attributes/>")
    else if children.length == 1 then Seq(s"<$name$attributes>" + children.head + s"</$name>")
    else Seq(s"<$name$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$name>")

  private def fromElement(
    element: Element,
    parent: Option[Element],
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

    val nodes: Nodes = atomize(Seq.empty, Element.getChildren(element))
    val whitespaceLeft: Boolean = nodes.headOption.exists(Atom.isWhitespace)
    val whitespaceRight: Boolean = nodes.lastOption.exists(Atom.isWhitespace)
    val charactersLeft: Boolean = nodes.headOption.exists(Atom.isCharacters)
    val charactersRight: Boolean = nodes.lastOption.exists(Atom.isCharacters)
    val chunks: Seq[Nodes] = chunkify(Seq.empty, Seq.empty, nodes, flush = false)
    val noText: Boolean = chunks.forall(_.forall(!Atom.is(_)))

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

    val label: String = Element.getName(element)
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

  private def getName(element: Element): String =
    Element.getPrefix(element).fold("")(_ + ":") + Element.getName(element)

  private def getAttributeValues(element: Element, parent: Option[Element]): Attribute.StringValues =
    val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(Namespace.getAll)
    Namespace.getAll(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
    Attribute.get(element).filterNot(_.value.isEmpty)

  @scala.annotation.tailrec
  private def atomize(result: Nodes, nodes: Nodes): Nodes =
    if nodes.isEmpty then result else
      val (texts: Nodes, tail: Nodes) = nodes.span(Atom.is)

      val newResult: Nodes = if texts.isEmpty then result else result ++
        processText(Seq.empty,
          Strings.squashBigWhitespace(texts.map(Atom.as).map(Atom.text).mkString("")))

      if tail.isEmpty then newResult else atomize(newResult :+ tail.head, tail.tail)

  @scala.annotation.tailrec
  private def processText(result: Seq[Atom], text: String): Seq[Atom] =
    if text.isEmpty then result else
      val (spaces: String, tail: String) = text.span(_ == ' ')
      val newResult = if spaces.isEmpty then result else result :+ Atom(" ")
      val (word: String, tail2: String) = tail.span(_ != ' ')

      if word.isEmpty then newResult else processText(newResult :+ Atom(word), tail2)

  @scala.annotation.tailrec
  private def chunkify(
    result: Seq[Nodes],
    current: Nodes,
    nodes: Nodes,
    flush: Boolean
  ): Seq[Nodes] =
    if flush then chunkify(result :+ current.reverse, Nil, nodes, flush = false) else (current, nodes) match
      case (Nil    , Nil    )                                          => result
      case (_      , Nil    )                                          => chunkify(result, current     , Nil     , flush = true )
      case (Nil    , n :: ns) if  Atom.isWhitespace(n)                 => chunkify(result, Nil         , ns      , flush = false)
      case (_      , n :: ns) if  Atom.isWhitespace(n)                 => chunkify(result, current     , ns      , flush = true )
      case (Nil    , n :: ns) if !Atom.isWhitespace(n)                 => chunkify(result, n :: Nil    , ns      , flush = false)
      case (c :: cs, n :: ns) if !Atom.isWhitespace(n) &&  cling(c, n) => chunkify(result, n :: c :: cs, ns      , flush = false)
      case (c :: _ , n :: ns) if !Atom.isWhitespace(n) && !cling(c, n) => chunkify(result, current     , n :: ns , flush = true )

  private def cling(c: Node, n: Node): Boolean = Atom.is(c) || Atom.is(n) ||
    (Element.is(n) && clingyElements.contains(Element.getName(Element.as(n))))

  private def fromChunk(
    nodes: Nodes,
    parent: Option[Element],
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

  private def fromPreformattedNode(node: Node, parent: Option[Element]): Seq[String] =
    if Element.is(node) then fromPreformattedElement(Element.as(node), parent)
    else if Atom.is(node) then preformattedLines(Atom.text(Atom.as(node)))
    else preformattedLines(Node.toString(node))

  private def fromNode(
    node: Node,
    parent: Option[Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    if Element.is(node) then
      val element: Element = Element.as(node)
      if preformattedElements.contains(Element.getName(element)) then
        Doc.text(fromPreformattedElement(element, parent).mkString(PrettyPrinter.hiddenNewline))
      else
        val result: Doc = fromElement(element, parent, canBreakLeft, canBreakRight)
        // Note: suppressing extra hardLine when lb is in stack is non-trivial - and not worth it :)
        if canBreakRight && Element.getName(element) == "lb" then result + Doc.hardLine else result
    else if Atom.is(node) then Doc.text(encodeXmlSpecials(Atom.text(Atom.as(node))))
    else Doc.paragraph(Node.toString(node))

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
