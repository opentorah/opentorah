package org.digitaljudaica.xml

import org.typelevel.paiges.Doc
import scala.xml.{Attribute, Elem, MetaData, NamespaceBinding, Node, SpecialNode, Text, TopScope, Utility}

/*
  scala.xml.PrettyPrinter breaks the line
   - between e1 and e2 in <e1>...</e1><e2>...</e2>
   - between e1 and text in: <e1>...</e1>text,
  separating notes from what they are notes on and punctuation from what its content by spaces.
  I decided to see if Wadler's "Prettier Printer" can help avoid ugly "fix" (Print.merge) and
  have more control over the result. Paiges is a Scala implementation of the "Prettier Printer",
  so I don't have to write my own :)
 */
final class PaigesPrettyPrinter(
  width: Int,
  indent: Int
) {
  import PaigesPrettyPrinter.sbToString

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
      // TODO suppress extra hardLine when in stack
      if (canBreakRight && element.label == "lb") result + Doc.hardLine else result

    case text: Text =>
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

    val (chunks: Seq[Doc], noAtoms: Boolean) = fromChildren(element, canBreakLeft, canBreakRight)

    if (chunks.isEmpty) {
      Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text("/>")

    // If this is clearly a bunch of elements - stack 'em with an indent:
    } else if (canBreakLeft && canBreakRight && noAtoms && (chunks.length >= 2)) {
        Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">") +
        Doc.cat(chunks.map(chunk => (Doc.hardLine + chunk).nested(indent))) +
        Doc.hardLine + Doc.text(s"</$name>")

    // If this is a paragraph (<p>) - nest its chunks unless there is only one:
    // TODO remove lineOrEmpty in chunks and right?
    } else if (canBreakLeft && canBreakRight && (element.label == "p") && (chunks.length >= 2)) {
      (Doc.lineOrEmpty + Doc.intercalate(Doc.lineOrSpace, chunks)).tightBracketBy(
        left = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">"),
        right = Doc.lineOrEmpty + Doc.text(s"</$name>"),
        indent
      )

    // Mixed content or non-break-off-able attachments on the side(s) cause flow-style:
    } else {
      Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">") +
      (if (canBreakLeft) Doc.lineOrEmpty else Doc.empty) +
      Doc.intercalate(Doc.lineOrSpace, chunks) +
      (if (canBreakRight) Doc.lineOrEmpty else Doc.empty) +
      Doc.text(s"</$name>")
    }
  }

  private def fromChildren(
    element: Elem,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): (Seq[Doc], Boolean) = {
    val nodes = PaigesPrettyPrinter.atomize(Seq.empty, element.child)
    val whitespaceLeft = nodes.headOption.exists(XmlUtil.isWhitespace)
    val whitespaceRight = nodes.lastOption.exists(XmlUtil.isWhitespace)
    val chunks: Seq[Seq[Node]] = PaigesPrettyPrinter.chunkify(Seq.empty, nodes)
    val noAtoms: Boolean = chunks.forall(_.forall(node => !XmlUtil.isAtom(node)))
    val result = fromChunks(
      chunks,
      element.scope,
      canBreakLeft = canBreakLeft || whitespaceLeft,
      canBreakRight = canBreakRight || whitespaceRight
    )
    (result, noAtoms)
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
}

object PaigesPrettyPrinter {

  private def fromAttributes(element: Elem, pscope: NamespaceBinding): Seq[Doc] = {
    val attributes: Seq[Doc] = element.attributes.toSeq.map(fromAttribute)
    val scopeStr: String = element.scope.buildString(pscope).trim
    if (scopeStr.isEmpty) attributes else attributes :+ Doc.text(scopeStr)
  }

  private def fromAttribute(attribute: MetaData): Doc = attribute match {
    case attribute: Attribute if attribute.value != null =>
      val key: Doc =
        Doc.text((if (attribute.isPrefixed) s"${attribute.pre}:" else "") + s"${attribute.key}=")
      val value = Doc.text(sbToString(Utility.appendQuoted(
        sbToString(Utility.sequenceToXML(attribute.value, TopScope, _, stripComments = true)), _)))
      key + value

    case _ =>
      Doc.empty
  }

  @scala.annotation.tailrec
  private def chunkify(result: Seq[Seq[Node]], nodes: Seq[Node]): Seq[Seq[Node]] = if (nodes.isEmpty) Seq.empty else {
    val (chunk: Seq[Node], tail: Seq[Node]) = nodes.dropWhile(XmlUtil.isWhitespace).span(node => !XmlUtil.isWhitespace(node))
    if (chunk.isEmpty) result else {
      val newResult = result ++ splitChunk(Seq.empty, Seq.empty, chunk)
      if (tail.forall(XmlUtil.isWhitespace)) newResult
      else chunkify(newResult, tail)
    }
  }

  @scala.annotation.tailrec
  private def splitChunk(result: Seq[Seq[Node]], current: Seq[Node], chunk: Seq[Node]): Seq[Seq[Node]] = chunk match {
    case Nil => result :+ current
    case n :: ns if XmlUtil.isText(n) || isNote(n) => splitChunk(result, current :+ n, ns)
    case n1 :: n2 :: ns if XmlUtil.isElement(n1) && XmlUtil.isText(n2) => splitChunk(result, current ++ Seq(n1, n2), ns)
    case n :: ns => splitChunk(if (current.isEmpty) result else result :+ current, Seq(n), ns)
  }

  private def isNote(node: Node): Boolean = XmlUtil.isElement(node) && (node.label == "note")

  @scala.annotation.tailrec
  private def atomize(result: Seq[Node], nodes: Seq[Node]): Seq[Node] = if (nodes.isEmpty) result else {
    val (atoms: Seq[Node], tail: Seq[Node]) = nodes.span(XmlUtil.isText)

    val newResult: Seq[Node] = if (atoms.isEmpty) result else {
      val text: String = atoms.map(_.asInstanceOf[Text].data).mkString("")
        .replace('\n', ' ')
        .replace('\t', ' ')

      result ++ processText(Seq.empty, text)
    }

    if (tail.isEmpty) newResult
    else atomize(newResult :+ tail.head, tail.tail)
  }

  @scala.annotation.tailrec
  private def processText(result: Seq[Text], text: String): Seq[Text] = if (text.isEmpty) result else {
    val (spaces: String, tail: String) = text.span(_ == ' ')
    val newResult = if (spaces.isEmpty) result else result :+ Text(" ")
    val (word: String, tail2: String) = tail.span(_ != ' ')

    if (word.isEmpty) newResult
    else processText(newResult :+ Text(word), tail2)
  }

  private def sbToString(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }
}
