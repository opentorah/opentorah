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

  // TODO add Doc.lineBreak after each chunk if there are more than one and there are no atoms in them.
  // TODO add Doc.lineBreak after </l> and <lb/> and before <l>.
  private def fromElement(element: Elem, pscope: NamespaceBinding): Doc = {
    val name: String = sbToString(element.nameToString)

    val header: Doc = {
      val attributes: Seq[Doc] = element.attributes.toSeq.map(fromAttribute)
      val scopeStr: String = element.scope.buildString(pscope).trim
      val docs = if (scopeStr.isEmpty) attributes else attributes :+ Doc.text(scopeStr)
      Doc.intercalate(Doc.lineOrSpace, docs)
    }

    val children: Doc = fromChildren(element.child, element.scope)

    // TODO make work tests that fail now with spurious spaces or lines (with spaces) in the output.
    if (header.isEmpty && children.isEmpty)
      Doc.text(s"<$name") + Doc.lineOrEmpty + Doc.text("/>")
    else if (children.isEmpty)
          header.tightBracketBy(Doc.text(s"<$name") + Doc.lineOrSpace, Doc.text("/>"), indent)
//      Doc.text(s"<$name") + Doc.lineOrSpace + header + Doc.lineOrEmpty + Doc.text("/>")
    else if (header.isEmpty) children.tightBracketBy(
      left = Doc.text(s"<$name") + Doc.lineOrEmpty + Doc.text(">"),
      right = Doc.text(s"</$name>"),
      indent
    ) else children.tightBracketBy(
      left = header.tightBracketBy(Doc.text(s"<$name") + Doc.lineOrSpace, Doc.text(">"), indent),
      right = Doc.text(s"</$name>"),
      indent
    )
  }

  // TODO preserve unbreakability when there was no whitespace before (same for after).
  private def fromChildren(nodes: Seq[Node], pscope: NamespaceBinding): Doc = {
    val whitespaceBefore = nodes.headOption.exists(XmlUtil.isWhitespace)
    val whitespaceAfter = nodes.lastOption.exists(XmlUtil.isWhitespace)
    val chunks: Seq[Seq[Node]] = chunkify(Seq.empty, nodes)

    if (chunks.isEmpty) Doc.empty else {
      Doc.intercalate(Doc.empty,
        chunks.map((nodes: Seq[Node]) =>
          Doc.intercalate(Doc.lineOrEmpty,
            nodes.map(fromNode(_, pscope))
          )
        )
      )
    }
  }

  @scala.annotation.tailrec
  private def chunkify(result: Seq[Seq[Node]], nodes: Seq[Node]): Seq[Seq[Node]] = {
    val (chunk, tail) = nodes.dropWhile(XmlUtil.isWhitespace).span(node => !XmlUtil.isWhitespace(node))
    require(chunk.nonEmpty || tail.isEmpty)
    if (chunk.isEmpty) result
    else if (tail.forall(XmlUtil.isWhitespace)) result :+ chunk
    else chunkify(result :+ chunk, tail)
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
}

object PaigesPrettyPrinter {

  private def sbToString(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }
}