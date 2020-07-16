package org.opentorah.xml

import org.opentorah.util.Strings
import org.opentorah.util.Strings.sbToString
import org.typelevel.paiges.Doc
import scala.xml.{Elem, MetaData, NamespaceBinding, Node, SpecialNode, TopScope, Utility}

final class PrettyPrinter(
  width: Int = 120,
  indent: Int = 2,
  doNotStackElements: Set[String] = Set(),
  allwaysStackElements: Set[String] = Set(),
  nestElements: Set[String] = Set(),
  clingyElements: Set[String] = Set()
) {
  private def isClingy(node: Node): Boolean = Xml.isElement(node) && clingyElements.contains(node.label)

  def renderXml(node: Node): String = Xml.header + "\n" + render(node) + "\n"

  def render(node: Node, pscope: NamespaceBinding = TopScope): String = fromNode(
    node,
    pscope,
    canBreakLeft = true,
    canBreakRight = true
  ).render(width)

  private def fromNode(
    node: Node,
    pscope: NamespaceBinding,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc = node match {
    case element: Elem =>
      val result = fromElement(element, pscope, canBreakLeft, canBreakRight)
      // Note: suppressing extra hardLine when lb is in stack is non-trivial - and not worth it :)
      if (canBreakRight && element.label == "lb") result + Doc.hardLine else result

    case text: scala.xml.Text =>
      Doc.text(text.data)

    case special: SpecialNode =>
      Doc.paragraph(sbToString(special.buildString))

    case node: Node =>
      Doc.paragraph(node.text)
  }

  private def fromElement(
    element: Elem,
    pscope: NamespaceBinding,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc = {
    val name: String = sbToString(element.nameToString)

    val attributes: Doc = {
      val docs: Seq[Doc] = PrettyPrinter.fromAttributes(element, pscope)
      if (docs.isEmpty) Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, docs)
    }

    val (chunks: Seq[Doc], noAtoms: Boolean, charactersLeft: Boolean, charactersRight: Boolean) =
      fromChildren(element, canBreakLeft, canBreakRight)

    if (chunks.isEmpty) Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text("/>") else  {
      val start: Doc = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">")
      val end: Doc = Doc.text(s"</$name>")

      val stackElements: Boolean = noAtoms &&
        ((chunks.length >= 2) || ((chunks.length == 1) && allwaysStackElements.contains(element.label))) &&
        !doNotStackElements.contains(element.label)
      if (stackElements) {
        // If this is clearly a bunch of elements - stack 'em with an indent:
        start +
        Doc.cat(chunks.map(chunk => (Doc.hardLine + chunk).nested(indent))) +
        Doc.hardLine + end
      } else if (nestElements.contains(element.label)) {
        // If this is forced-nested element - nest it:
        Doc.intercalate(Doc.lineOrSpace, chunks).tightBracketBy(
          left = start,
          right = end,
          indent
        )
      } else {
        // Mixed content or non-break-off-able attachments on the side(s) cause flow-style;
        // character content should stick to the opening and closing tags:
        start +
        (if (canBreakLeft && !charactersLeft) Doc.lineOrEmpty else Doc.empty) +
        Doc.intercalate(Doc.lineOrSpace, chunks) +
        (if (canBreakRight && !charactersRight) Doc.lineOrEmpty else Doc.empty) +
        end
      }
    }
  }

  private def fromChildren(
    element: Elem,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): (Seq[Doc], Boolean, Boolean, Boolean) = {
    val nodes: Seq[Node] = PrettyPrinter.atomize(Seq.empty, element.child)
    val whitespaceLeft: Boolean = nodes.headOption.exists(Xml.isWhitespace)
    val whitespaceRight: Boolean = nodes.lastOption.exists(Xml.isWhitespace)
    val charactersLeft: Boolean = nodes.headOption.exists(node => Xml.isAtom(node) && !Xml.isWhitespace(node))
    val charactersRight: Boolean = nodes.lastOption.exists(node => Xml.isAtom(node) && !Xml.isWhitespace(node))
    val chunks: Seq[Seq[Node]] = chunkify(Seq.empty, Seq.empty, nodes)
    val noAtoms: Boolean = chunks.forall(_.forall(node => !Xml.isAtom(node)))
    val result = fromChunks(
      chunks,
      element.scope,
      canBreakLeft = canBreakLeft || whitespaceLeft,
      canBreakRight = canBreakRight || whitespaceRight
    )
    (result, noAtoms, charactersLeft, charactersRight)
  }

  private def fromChunks(
    chunks: Seq[Seq[Node]],
    pscope: NamespaceBinding,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Seq[Doc] = {
    if (chunks.isEmpty) Seq.empty
    else if (chunks.length == 1) Seq(
      fromChunk(chunks.head, pscope, canBreakLeft, canBreakRight)
    ) else {
      fromChunk(chunks.head, pscope, canBreakLeft = canBreakLeft, canBreakRight = true) +:
      chunks.tail.init.map(chunk => fromChunk(chunk, pscope, canBreakLeft = true, canBreakRight = true)) :+
      fromChunk(chunks.last, pscope, canBreakLeft = true, canBreakRight = canBreakRight)
    }
  }

  private def fromChunk(
    nodes: Seq[Node],
    pscope: NamespaceBinding,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc = {
    require(nodes.nonEmpty)
    if (nodes.length == 1) {
      fromNode(nodes.head, pscope, canBreakLeft, canBreakRight)
    } else {
      val result: Seq[Doc] =
        fromNode(nodes.head, pscope, canBreakLeft, canBreakRight = false) +:
        nodes.tail.init.map(node => fromNode(node, pscope, canBreakLeft = false, canBreakRight = false)) :+
        fromNode(nodes.last, pscope, canBreakLeft = false, canBreakRight)

      Doc.intercalate(Doc.empty, result)
    }
  }

  private def chunkify(result: Seq[Seq[Node]], current: Seq[Node], nodes: Seq[Node]): Seq[Seq[Node]] = {
    def cling(c: Node, n: Node): Boolean = Xml.isText(c) || Xml.isText(n) || isClingy(n)
    def flush(nodes: Seq[Node]): Seq[Seq[Node]] = chunkify(result :+ current.reverse, Nil, nodes)

    (current, nodes) match {
      case (Nil    , Nil    ) => result
      case (c :: cs, Nil    ) => flush(Nil)
      case (Nil    , n :: ns) if  Xml.isWhitespace(n) => chunkify(result, Nil, ns)
      case (c :: cs, n :: ns) if  Xml.isWhitespace(n) => flush(ns)
      case (Nil    , n :: ns) if !Xml.isWhitespace(n) => chunkify(result, n :: Nil, ns)
      case (c :: cs, n :: ns) if !Xml.isWhitespace(n) &&  cling(c, n) => chunkify(result, n :: c :: cs, ns)
      case (c :: cs, n :: ns) if !Xml.isWhitespace(n) && !cling(c, n) => flush(n :: ns)
    }
  }
}

object PrettyPrinter {

  private def fromAttributes(element: Elem, pscope: NamespaceBinding): Seq[Doc] = {
    val attributes: Seq[Doc] = element.attributes.toSeq.map(fromAttribute)
    val scopeStr: String = element.scope.buildString(pscope).trim
    if (scopeStr.isEmpty) attributes else attributes :+ Doc.text(scopeStr)
  }

  private def fromAttribute(attribute: MetaData): Doc = attribute match {
    case attribute: scala.xml.Attribute if attribute.value != null =>
      val key: Doc =
        Doc.text((if (attribute.isPrefixed) s"${attribute.pre}:" else "") + s"${attribute.key}=")
      val value = Doc.text(sbToString(Utility.appendQuoted(
        sbToString(Utility.sequenceToXML(attribute.value, TopScope, _, stripComments = true)), _)))
      key + value

    case _ =>
      Doc.empty
  }

  @scala.annotation.tailrec
  private def atomize(result: Seq[Node], nodes: Seq[Node]): Seq[Node] = if (nodes.isEmpty) result else {
    val (atoms: Seq[Node], tail: Seq[Node]) = nodes.span(Xml.isText)

    val newResult: Seq[Node] = if (atoms.isEmpty) result else result ++
      processText(Seq.empty, Strings.squashBigWhitespace(atoms.map(_.asInstanceOf[scala.xml.Text].data).mkString("")))

    if (tail.isEmpty) newResult
    else atomize(newResult :+ tail.head, tail.tail)
  }

  @scala.annotation.tailrec
  private def processText(result: Seq[scala.xml.Text], text: String): Seq[scala.xml.Text] = if (text.isEmpty) result else {
    val (spaces: String, tail: String) = text.span(_ == ' ')
    val newResult = if (spaces.isEmpty) result else result :+ scala.xml.Text(" ")
    val (word: String, tail2: String) = tail.span(_ != ' ')

    if (word.isEmpty) newResult
    else processText(newResult :+ scala.xml.Text(word), tail2)
  }
}
