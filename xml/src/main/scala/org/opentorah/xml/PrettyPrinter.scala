package org.opentorah.xml

import org.opentorah.util.Strings
import org.typelevel.paiges.Doc

final class PrettyPrinter(
  width: Int = 120,
  indent: Int = 2,
  doNotStackElements: Set[String] = Set(),
  allwaysStackElements: Set[String] = Set(),
  nestElements: Set[String] = Set(),
  clingyElements: Set[String] = Set()
) {
  // Note: making entry points generic in N and taking implicit N: Model[N] does not work,
  // since what is passed in for the 'node' parameter is usually an Elem, but Xml: Model[Node],
  // so implicit search fails - unless value for the 'node' parameter is passed in with ascription: '...: Node';
  // it is cleaner to just provide Model-specific entry points...

  def renderXml(node: scala.xml.Node, doctype: Option[String] = None): String =
    mkRun(Xml).renderXml(node, doctype)

  def render(node: scala.xml.Node): String =
    mkRun(Xml).render(node)

  private def mkRun[N](N: Model[N]): PrettyPrinter.Run[N] = new PrettyPrinter.Run(
    width,
    indent,
    doNotStackElements,
    allwaysStackElements,
    nestElements,
    clingyElements
  )(N)
}

object PrettyPrinter {

  // Note: methods below all need access to N: Model[N], often - for the types of parameters, so parameter N
  // must come first, and can not be implicit; it is cleaner to scope them in a class with N a constructor parameter.
  private final class Run[N](
    width: Int,
    indent: Int,
    doNotStackElements: Set[String],
    allwaysStackElements: Set[String],
    nestElements: Set[String],
    clingyElements: Set[String]
  )(N: Model[N]) {

    def renderXml(node: N, doctype: Option[String] = None): String =
      Xml.header + "\n" +
      doctype.fold("")(doctype => doctype + "\n") +
      render(node) + "\n"

    def render(node: N): String = fromNode(
      node,
      namespaces = Seq(Namespace.Top),
      canBreakLeft = true,
      canBreakRight = true
    ).render(width)

    private def fromNode(
      node: N,
      namespaces: Seq[Namespace],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc = {
      if (N.isElement(node)) {
        val element: N.Element = N.asElement(node)
        val result = fromElement(element, namespaces, canBreakLeft, canBreakRight)
        // Note: suppressing extra hardLine when lb is in stack is non-trivial - and not worth it :)
        if (canBreakRight && N.getName(element) == "lb") result + Doc.hardLine else result
      }
      else if (N.isText(node)) Doc.text(N.getText(N.asText(node)))
      else Doc.paragraph(N.getNodeText(node))
    }

    private def fromElement(
      element: N.Element,
      namespaces: Seq[Namespace],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc = {
      val name: String = N.getPrefix(element).fold("")(_ + ":") + N.getName(element)
      val elementNamespaces: Seq[Namespace] = N.getNamespaces(element)

      val attributes: Doc = {
        val namespaceAttributes: Seq[Attribute.Value[String]] = elementNamespaces.flatMap(elementNamespace =>
          if ((elementNamespace == Namespace.Top) || namespaces.contains(elementNamespace)) None
          else Some(elementNamespace.xmlnsAttribute))

        val docs: Seq[Doc] =
          (N.getAttributes(element) ++ namespaceAttributes).filterNot(_.value.isEmpty).map(attributeValue =>
            // TODO use '' instead of "" if value contains "?
            Doc.text(attributeValue.attribute.prefixedName + "=" + "\"" + attributeValue.value.get + "\"")
          )

        if (docs.isEmpty) Doc.empty
        else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, docs)
      }

      val (chunks: Seq[Doc], noAtoms: Boolean, charactersLeft: Boolean, charactersRight: Boolean) =
        fromChildren(element, elementNamespaces, canBreakLeft, canBreakRight)

      if (chunks.isEmpty) Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text("/>") else  {
        val start: Doc = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">")
        val end: Doc = Doc.text(s"</$name>")

        val label: String = N.getName(element)

        val stackElements: Boolean = noAtoms &&
          ((chunks.length >= 2) || ((chunks.length == 1) && allwaysStackElements.contains(label))) &&
          !doNotStackElements.contains(label)

        if (stackElements) {
          // If this is clearly a bunch of elements - stack 'em with an indent:
          start +
          Doc.cat(chunks.map(chunk => (Doc.hardLine + chunk).nested(indent))) +
          Doc.hardLine + end
        } else if (nestElements.contains(label)) {
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
      element: N.Element,
      elementNamespaces: Seq[Namespace],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): (Seq[Doc], Boolean, Boolean, Boolean) = {
      val nodes: Seq[N] = atomize(Seq.empty, N.getChildren(element))
      val whitespaceLeft: Boolean = nodes.headOption.exists(N.isWhitespace)
      val whitespaceRight: Boolean = nodes.lastOption.exists(N.isWhitespace)
      val charactersLeft: Boolean = nodes.headOption.exists(node => N.isAtom(node) && !N.isWhitespace(node))
      val charactersRight: Boolean = nodes.lastOption.exists(node => N.isAtom(node) && !N.isWhitespace(node))
      val chunks: Seq[Seq[N]] = chunkify(Seq.empty, Seq.empty, nodes)
      val noAtoms: Boolean = chunks.forall(_.forall(node => !N.isAtom(node)))
      val canBreakLeft1 = canBreakLeft || whitespaceLeft
      val canBreakRight1 = canBreakRight || whitespaceRight
      val result: Seq[Doc] =
        if (chunks.isEmpty) Seq.empty
        else if (chunks.length == 1) Seq(
          fromChunk(chunks.head, elementNamespaces, canBreakLeft1, canBreakRight1)
        ) else {
          fromChunk(chunks.head, elementNamespaces, canBreakLeft = canBreakLeft1, canBreakRight = true) +:
          chunks.tail.init.map(chunk => fromChunk(chunk, elementNamespaces, canBreakLeft = true, canBreakRight = true)) :+
          fromChunk(chunks.last, elementNamespaces, canBreakLeft = true, canBreakRight = canBreakRight1)
        }
      (result, noAtoms, charactersLeft, charactersRight)
    }

    private def fromChunk(
      nodes: Seq[N],
      namespaces: Seq[Namespace],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc = {
      require(nodes.nonEmpty)
      if (nodes.length == 1) {
        fromNode(nodes.head, namespaces, canBreakLeft, canBreakRight)
      } else {
        val result: Seq[Doc] =
          fromNode(nodes.head, namespaces, canBreakLeft, canBreakRight = false) +:
          nodes.tail.init.map(node => fromNode(node, namespaces, canBreakLeft = false, canBreakRight = false)) :+
          fromNode(nodes.last, namespaces, canBreakLeft = false, canBreakRight)

        Doc.intercalate(Doc.empty, result)
      }
    }

    private def chunkify(
      result: Seq[Seq[N]],
      current: Seq[N],
      nodes: Seq[N]
    ): Seq[Seq[N]] = {
      def cling(c: N, n: N): Boolean = N.isText(c) || N.isText(n) ||
        (N.isElement(n) && clingyElements.contains(N.getName(N.asElement(n))))

      def flush(nodes: Seq[N]): Seq[Seq[N]] = chunkify(result :+ current.reverse, Nil, nodes)

      (current, nodes) match {
        case (Nil    , Nil    ) => result
        case (c :: cs, Nil    ) => flush(Nil)
        case (Nil    , n :: ns) if  N.isWhitespace(n) => chunkify(result, Nil, ns)
        case (c :: cs, n :: ns) if  N.isWhitespace(n) => flush(ns)
        case (Nil    , n :: ns) if !N.isWhitespace(n) => chunkify(result, n :: Nil, ns)
        case (c :: cs, n :: ns) if !N.isWhitespace(n) &&  cling(c, n) => chunkify(result, n :: c :: cs, ns)
        case (c :: cs, n :: ns) if !N.isWhitespace(n) && !cling(c, n) => flush(n :: ns)
      }
    }

    @scala.annotation.tailrec
    private def atomize(result: Seq[N], nodes: Seq[N]): Seq[N] = if (nodes.isEmpty) result else {
      val (atoms: Seq[N], tail: Seq[N]) = nodes.span(N.isText)

      val newResult: Seq[N] = if (atoms.isEmpty) result else result ++
        processText(Seq.empty, Strings.squashBigWhitespace(atoms.map(N.asText).map(N.getText).mkString("")))

      if (tail.isEmpty) newResult
      else atomize(newResult :+ tail.head, tail.tail)
    }

    @scala.annotation.tailrec
    private def processText(result: Seq[N.Text], text: String): Seq[N.Text] = if (text.isEmpty) result else {
      val (spaces: String, tail: String) = text.span(_ == ' ')
      val newResult = if (spaces.isEmpty) result else result :+ N.mkText(" ")
      val (word: String, tail2: String) = tail.span(_ != ' ')

      if (word.isEmpty) newResult
      else processText(newResult :+ N.mkText(word), tail2)
    }
  }

  val default: PrettyPrinter = new PrettyPrinter

  def render(node: org.w3c.dom.Node): String = serializer.writeToString(node)

  private val serializer: org.apache.xml.serializer.dom3.LSSerializerImpl = {
    val result = new org.apache.xml.serializer.dom3.LSSerializerImpl
    result.setParameter("format-pretty-print", true)
    result
  }
}
