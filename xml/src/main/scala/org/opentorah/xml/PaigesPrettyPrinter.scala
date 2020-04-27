package org.opentorah.xml

import org.typelevel.paiges.Doc
import scala.xml.{Elem, MetaData, NamespaceBinding, Node, SpecialNode, TopScope, Utility}

final class PaigesPrettyPrinter(
  width: Int = 120,
  indent: Int = 2,
  doNotStackElements: Set[String] = Set(),
  nestElements: Set[String] = Set(),
  clingyElements: Set[String] = Set()
) {
  import PaigesPrettyPrinter.sbToString

  private def isClingy(node: Node): Boolean = Xml.isElement(node) && clingyElements.contains(node.label)

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
      val docs: Seq[Doc] = PaigesPrettyPrinter.fromAttributes(element, pscope)
      if (docs.isEmpty) Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, docs)
    }

    val (chunks: Seq[Doc], noAtoms: Boolean, charactersLeft: Boolean, charactersRight: Boolean) =
      fromChildren(element, canBreakLeft, canBreakRight)

    if (chunks.isEmpty) Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text("/>") else  {
      val start: Doc = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">")
      val end: Doc = Doc.text(s"</$name>")

      if (noAtoms && (chunks.length >= 2) && !doNotStackElements.contains(element.label)) {
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
    val nodes: Seq[Node] = PaigesPrettyPrinter.atomize(Seq.empty, element.child)
    val whitespaceLeft: Boolean = nodes.headOption.exists(Xml.isWhitespace)
    val whitespaceRight: Boolean = nodes.lastOption.exists(Xml.isWhitespace)
    val charactersLeft: Boolean = nodes.headOption.exists(node => Xml.isAtom(node) && !Xml.isWhitespace(node))
    val charactersRight: Boolean = nodes.lastOption.exists(node => Xml.isAtom(node) && !Xml.isWhitespace(node))
    val chunks: Seq[Seq[Node]] = chunkify(Seq.empty, nodes)
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

  @scala.annotation.tailrec
  private def chunkify(result: Seq[Seq[Node]], nodes: Seq[Node]): Seq[Seq[Node]] = if (nodes.isEmpty) Seq.empty else {
    val (chunk: Seq[Node], tail: Seq[Node]) = nodes.dropWhile(Xml.isWhitespace).span(node => !Xml.isWhitespace(node))
    if (chunk.isEmpty) result else {
      val newResult = result ++ splitChunk(chunk)
      if (tail.forall(Xml.isWhitespace)) newResult
      else chunkify(newResult, tail)
    }
  }

  private def splitChunk(chunk: Seq[Node]): Seq[Seq[Node]] =
    if (chunk.isEmpty) Seq.empty else splitChunk(Seq.empty, Seq.empty, chunk)

  @scala.annotation.tailrec
  private def splitChunk(result: Seq[Seq[Node]], current: Seq[Node], chunk: Seq[Node]): Seq[Seq[Node]] =
  // If chunk was empty to begin with, this recursive function doesn't get called (see above);
  // otherwise, current is never empty here, so we are guaranteed to not return empty sequences.
    if (chunk.isEmpty) result :+ current else {
      // Head of the chunk always gets included; breaks are possible only after it
      val head = chunk.head
      val tail = chunk.tail
      val newCurrent = current :+ head

      if (tail.isEmpty) result :+ newCurrent else {
        val next = tail.head

        if (Xml.isText(head) || Xml.isText(next) || isClingy(next))
          splitChunk(result, newCurrent, tail)
        else
          splitChunk(result :+ newCurrent, Seq.empty, tail)
      }
    }
}

object PaigesPrettyPrinter {

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

    val newResult: Seq[Node] = if (atoms.isEmpty) result else {
      val text: String = atoms.map(_.asInstanceOf[scala.xml.Text].data).mkString("")
        .replace('\n', ' ')
        .replace('\t', ' ')

      result ++ processText(Seq.empty, text)
    }

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

  private def sbToString(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }
}
