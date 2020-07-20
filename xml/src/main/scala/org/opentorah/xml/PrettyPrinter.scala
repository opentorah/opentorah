package org.opentorah.xml

import org.opentorah.util.Strings
import org.typelevel.paiges.Doc

final class PrettyPrinter(
  width: Int = 120,
  indent: Int = 2,
  doNotStackElements: Set[String] = Set(),
  alwaysStackElements: Set[String] = Set(),
  nestElements: Set[String] = Set(),
  clingyElements: Set[String] = Set()
) {
  // Note: making entry points generic in N and taking implicit N: Model[N] does not work,
  // since 'elem' parameter is of type Elem, but Xml: Model[Node];
  // it is cleaner to just provide Model-specific entry points...

  def renderXml(element: scala.xml.Elem, doctype: String): String =
    renderXml(element, Some(doctype))

  def renderXml(element: scala.xml.Elem): String =
    renderXml(element, None)

  def renderXml(element: scala.xml.Elem, doctype: Option[String]): String = {
    val run = mkRun(Xml)
    run.renderXml(element.asInstanceOf[run.N.Element], doctype)
  }

  def render(element: scala.xml.Elem): String = {
    val run = mkRun(Xml)
    run.render(element.asInstanceOf[run.N.Element])
  }


  def renderXml(element: org.w3c.dom.Element, doctype: String): String =
    renderXml(element, Some(doctype))

  def renderXml(element: org.w3c.dom.Element): String =
    renderXml(element, None)

  private def renderXml(element: org.w3c.dom.Element, doctype: Option[String]): String = {
    val run = mkRun(Dom)
    run.renderXml(element.asInstanceOf[run.N.Element], doctype)
  }

  def render(element: org.w3c.dom.Element): String = {
    val run = mkRun(Dom)
    run.render(element.asInstanceOf[run.N.Element])
  }

  private def mkRun[N](N: Model[N]): PrettyPrinter.Run[N] = new PrettyPrinter.Run(N)(
    width,
    indent,
    doNotStackElements,
    alwaysStackElements,
    nestElements,
    clingyElements
  )
}

object PrettyPrinter {

  // Note: methods below all need access to N: Model[N], often - for the types of parameters, so parameter N
  // must come first, and can not be implicit; it is cleaner to scope them in a class with N a constructor parameter.
  private final class Run[N](val N: Model[N])(
    width: Int,
    indent: Int,
    doNotStackElements: Set[String],
    alwaysStackElements: Set[String],
    nestElements: Set[String],
    clingyElements: Set[String]
  ) {

    def renderXml(node: N.Element, doctype: Option[String]): String =
      Namespace.Xml.header + "\n" +
      doctype.fold("")(doctype => doctype + "\n") +
      render(node) + "\n"

    def render(element: N.Element): String = fromElement(
      element,
      parent = None,
      canBreakLeft = true,
      canBreakRight = true
    ).render(width)

    private def fromElement(
      element: N.Element,
      parent: Option[N.Element],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc = {
      val label: String = N.getName(element)
      val name: String = N.getPrefix(element).fold("")(_ + ":") + label

      val attributeValues: Seq[Attribute.Value[String]] = N.getAttributes(element, parent).filterNot(_.value.isEmpty)
      val attributes: Doc =
        if (attributeValues.isEmpty) Doc.empty
        else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, attributeValues.map(attributeValue =>
          Doc.text(attributeValue.attribute.prefixedName + "=") + Doc.lineOrEmpty +
          Doc.text("\"" + attributeValue.value.get + "\"")
        ))

      val nodes: Seq[N] = atomize(Seq.empty, N.getChildren(element))
      val whitespaceLeft: Boolean = nodes.headOption.exists(N.isWhitespace)
      val whitespaceRight: Boolean = nodes.lastOption.exists(N.isWhitespace)
      val charactersLeft: Boolean = nodes.headOption.exists(N.isCharacters)
      val charactersRight: Boolean = nodes.lastOption.exists(N.isCharacters)
      val chunks: Seq[Seq[N]] = chunkify(Seq.empty, Seq.empty, nodes)
      val noText: Boolean = chunks.forall(_.forall(!N.isText(_)))

      val children: Seq[Doc] = {
        val canBreakLeft1 = canBreakLeft || whitespaceLeft
        val canBreakRight1 = canBreakRight || whitespaceRight

        if (chunks.isEmpty) Seq.empty
        else if (chunks.length == 1) Seq(
          fromChunk(chunks.head, Some(element), canBreakLeft1, canBreakRight1)
        ) else {
          fromChunk(chunks.head, Some(element), canBreakLeft = canBreakLeft1, canBreakRight = true) +:
          chunks.tail.init.map(chunk => fromChunk(chunk, Some(element), canBreakLeft = true, canBreakRight = true)) :+
          fromChunk(chunks.last, Some(element), canBreakLeft = true, canBreakRight = canBreakRight1)
        }
      }

      if (children.isEmpty) Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text("/>") else {
        val start: Doc = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">")
        val end: Doc = Doc.text(s"</$name>")

        val stackElements: Boolean = noText &&
          ((children.length >= 2) || ((children.length == 1) && alwaysStackElements.contains(label))) &&
          !doNotStackElements.contains(label)

        if (stackElements) {
          // If this is clearly a bunch of elements - stack 'em with an indent:
          start +
          Doc.cat(children.map(child => (Doc.hardLine + child).nested(indent))) +
          Doc.hardLine + end
        } else if (nestElements.contains(label)) {
          // If this is forced-nested element - nest it:
          Doc.intercalate(Doc.lineOrSpace, children).tightBracketBy(left = start, right = end, indent)
        } else {
          // Mixed content or non-break-off-able attachments on the side(s) cause flow-style;
          // character content should stick to the opening and closing tags:
          start +
          (if (canBreakLeft && !charactersLeft) Doc.lineOrEmpty else Doc.empty) +
          Doc.intercalate(Doc.lineOrSpace, children) +
          (if (canBreakRight && !charactersRight) Doc.lineOrEmpty else Doc.empty) +
          end
        }
      }
    }

    @scala.annotation.tailrec
    private def atomize(result: Seq[N], nodes: Seq[N]): Seq[N] = if (nodes.isEmpty) result else {
      val (texts: Seq[N], tail: Seq[N]) = nodes.span(N.isText)

      val newResult: Seq[N] = if (texts.isEmpty) result else result ++
        processText(Seq.empty, texts.head,
          Strings.squashBigWhitespace(texts.map(N.asText).map(N.getText).mkString("")))

      if (tail.isEmpty) newResult
      else atomize(newResult :+ tail.head, tail.tail)
    }

    @scala.annotation.tailrec
    private def processText(result: Seq[N.Text], seed: N, text: String): Seq[N.Text] = if (text.isEmpty) result else {
      val (spaces: String, tail: String) = text.span(_ == ' ')
      val newResult = if (spaces.isEmpty) result else result :+ N.mkText(" ", seed)
      val (word: String, tail2: String) = tail.span(_ != ' ')

      if (word.isEmpty) newResult
      else processText(newResult :+ N.mkText(word, seed), seed, tail2)
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

    private def fromChunk(
      nodes: Seq[N],
      parent: Option[N.Element],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc = {
      require(nodes.nonEmpty)
      if (nodes.length == 1) {
        fromNode(nodes.head, parent, canBreakLeft, canBreakRight)
      } else Doc.intercalate(Doc.empty, {
        fromNode(nodes.head, parent, canBreakLeft, canBreakRight = false) +:
        nodes.tail.init.map(node => fromNode(node, parent, canBreakLeft = false, canBreakRight = false)) :+
        fromNode(nodes.last, parent, canBreakLeft = false, canBreakRight)
      })
    }

    private def fromNode(
      node: N,
      parent: Option[N.Element],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc = {
      if (N.isElement(node)) {
        val element: N.Element = N.asElement(node)
        val result = fromElement(element, parent, canBreakLeft, canBreakRight)
        // Note: suppressing extra hardLine when lb is in stack is non-trivial - and not worth it :)
        if (canBreakRight && N.getName(element) == "lb") result + Doc.hardLine else result
      }
      else if (N.isText(node)) Doc.text(N.getText(N.asText(node)))
      else Doc.paragraph(N.toString(node))
    }
  }

  val default: PrettyPrinter = new PrettyPrinter

  // Note: to use LSSerializer instead of my DOM pretty-printing:
  // add dependency:  implementation "xalan:serializer:$xalanVersion"
  //
  // def render(node: org.w3c.dom.Node): String = serializer.writeToString(node)
  // private val serializer: org.apache.xml.serializer.dom3.LSSerializerImpl = {
  //  val result = new org.apache.xml.serializer.dom3.LSSerializerImpl
  //  result.setParameter("format-pretty-print", true)
  //  result
  //}
}
