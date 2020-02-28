package org.digitaljudaica.xml

import org.typelevel.paiges.Doc
import scala.xml.{Attribute, Elem, MetaData, NamespaceBinding, Node, SpecialNode, TopScope, Utility}

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
  indent: Int,
  width: Int
) {
  import PaigesPrettyPrinter.sbToString

  def render(node: Node, pscope: NamespaceBinding = TopScope): String =
    fromNode(node, pscope).render(width)

  def fromNode(node: Node, pscope: NamespaceBinding = TopScope): Doc = node match {
    case element: Elem =>
      fromElement(element, pscope)

    case special: SpecialNode =>
      Doc.paragraph(sbToString(special.buildString))

    case node: Node =>
      Doc.paragraph(node.text)
  }

  private def fromElement(element: Elem, pscope: NamespaceBinding): Doc = {
    val name: String = sbToString(element.nameToString)

    val attributes: Doc = {
      val docs: Seq[Doc] = PaigesPrettyPrinter.fromAttributes(element, pscope)
      if (docs.isEmpty) Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, docs)
    }

    val children: Doc = fromChildren(element.child, element.scope)

    if (children.isEmpty)
      Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text("/>")
    else children.tightBracketBy(
      left = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">"),
      right = Doc.text(s"</$name>"),
      indent
    )
  }

  // TODO preserve unbreakability when there was no whitespace before (same for after).
  // TODO add Doc.lineBreak after each chunk if there are more than one and there are no atoms in them.
  // TODO add Doc.lineBreak after </l> and <lb/> and before <l>.
  private def fromChildren(nodes: Seq[Node], pscope: NamespaceBinding): Doc = {
    def fromChunk(nodes: Seq[Node]): Doc = Doc.intercalate(Doc.empty, nodes.map(fromNode(_, pscope)))

    val whitespaceBefore = nodes.headOption.exists(XmlUtil.isWhitespace)
    val whitespaceAfter = nodes.lastOption.exists(XmlUtil.isWhitespace)
    val chunks: Seq[Seq[Node]] = PaigesPrettyPrinter.chunkify(Seq.empty, nodes)

    if (chunks.isEmpty) Doc.empty else {
      val noAtoms: Boolean = chunks.forall(_.forall(node => !XmlUtil.isAtom(node)))
      val hardLine: Boolean = false // (chunks.size >= 2) && noAtoms // TODO propagate upstream!
      // println(chunks.map(_.map(_.label).mkString("[", ", ", "]")).mkString(";"))
      // println(s"hardLine: $hardLine")

      if (hardLine) Doc.hardLine + Doc.intercalate(Doc.hardLine, chunks.map(fromChunk)) + Doc.hardLine
      else Doc.intercalate(Doc.lineOrEmpty, chunks.map(fromChunk))
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
  private def chunkify(result: Seq[Seq[Node]], nodes: Seq[Node]): Seq[Seq[Node]] = {
    val (chunk, tail) = nodes.dropWhile(XmlUtil.isWhitespace).span(node => !XmlUtil.isWhitespace(node))
    require(chunk.nonEmpty || tail.isEmpty)
    if (chunk.isEmpty) result
    else if (tail.forall(XmlUtil.isWhitespace)) result :+ chunk
    else chunkify(result :+ chunk, tail)
  }

  private def sbToString(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }
}