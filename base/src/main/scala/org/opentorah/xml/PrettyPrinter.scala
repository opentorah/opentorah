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
) {
  def renderXml(element: Xml.Element): String = renderXml(element, doctype = None)
  def renderXml(element: Xml.Element, doctype: Doctype): String = renderXml(element, doctype = Some(doctype))
  def renderXml(element: Xml.Element, doctype: Option[Doctype]): String = render(Xml)(element, addXmlHeader = true, doctype)

  def write(
    file: File,
    replace: Boolean,
    doctype: Option[Doctype] = None,
    elem: Xml.Element
  ): Unit = Files.write(
    file,
    replace,
    content = renderXml(element = elem, doctype)
  )

  def render(element: Xml.Element): String = render(element, doctype = None)
  def render(element: Xml.Element, doctype: Doctype): String = render(element, doctype = Some(doctype))
  def render(element: Xml.Element, doctype: Option[Doctype]): String = render(Xml)(element, addXmlHeader = false, doctype)

  def renderXml(element: Dom.Element): String = renderXml(element, doctype = None)
  def renderXml(element: Dom.Element, doctype: Doctype): String = renderXml(element, doctype = Some(doctype))
  def renderXml(element: Dom.Element, doctype: Option[Doctype]): String = render(Dom)(element, addXmlHeader = true, doctype)
  def render(element: Dom.Element): String = render(element, doctype = None)
  def render(element: Dom.Element, doctype: Doctype): String = render(element, doctype = Some(doctype))
  def render(element: Dom.Element, doctype: Option[Doctype]): String = render(Dom)(element, addXmlHeader = false, doctype)

  private def render(
    model: Model,
  )(
    element: model.Element,
    addXmlHeader: Boolean,
    doctype: Option[Doctype]
  ): String = {
    val forModel: ForModel = new ForModel(model)

    val result: String = forModel.fromElement(
      element.asInstanceOf[forModel.model.Element], // yuck...
      parent = None,
      canBreakLeft = true,
      canBreakRight = true
    )
      .render(width)
      .replace(PrettyPrinter.hiddenNewline, "\n")

    (if (!addXmlHeader) "" else Xml.header + "\n") +
    doctype.fold("")(doctype => doctype.doctype + "\n") +
    result + "\n"
  }

  // Note: methods below all need access to model: Model, often - for the types of parameters, so parameter model
  // must come first, and thus can not be implicit; it is cleaner to scope them in a class with model a constructor parameter.
  private final class ForModel(val model: Model) {

    private def fromPreformattedElement(element: model.Element, parent: Option[model.Element]): Seq[String] = {
      val attributeValues: Seq[Attribute.Value[String]] = getAttributeValues(element, parent)
      val attributes: String = if (attributeValues.isEmpty) "" else attributeValues
        .map(attributeValue =>  attributeValue.attribute.qName + "=\"" + attributeValue.valueToString.get + "\"")
        .mkString(" ", ", ", "")

      val children: Seq[String] =
        model.getChildren(element).flatMap(node => fromPreformattedNode(node, Some(element)))

      val name: String = getName(element)

      if (children.isEmpty) Seq(s"<$name$attributes/>")
      else if (children.length == 1) Seq(s"<$name$attributes>" + children.head + s"</$name>")
      else Seq(s"<$name$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$name>")
    }

    def fromElement(
      element: model.Element,
      parent: Option[model.Element],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc = {
      val attributeValues: Seq[Attribute.Value[String]] = getAttributeValues(element, parent)

      val attributes: Doc =
        if (attributeValues.isEmpty) Doc.empty
        else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, attributeValues.map(attributeValue =>
          Doc.text(attributeValue.attribute.qName + "=") + Doc.lineOrEmpty +
          // Note: maybe use single quotes if the value contains double quote?
          Doc.text("\"" + encodeXmlSpecials(attributeValue.valueToString.get) + "\"")
        ))

      val nodes: model.Nodes = atomize(Seq.empty, model.getChildren(element))
      val whitespaceLeft: Boolean = nodes.headOption.exists(model.isWhitespace)
      val whitespaceRight: Boolean = nodes.lastOption.exists(model.isWhitespace)
      val charactersLeft: Boolean = nodes.headOption.exists(model.isCharacters)
      val charactersRight: Boolean = nodes.lastOption.exists(model.isCharacters)
      val chunks: Seq[model.Nodes] = chunkify(Seq.empty, Seq.empty, nodes, flush = false)
      val noText: Boolean = chunks.forall(_.forall(!model.isText(_)))

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

      val label: String = model.getName(element)
      val name: String = getName(element)

      if (children.isEmpty) {
        Doc.text(s"<$name") + attributes + Doc.lineOrEmpty +
          (if (allowEmptyElements || keepEmptyElements.contains(name)) Doc.text("/>") else Doc.text(s"></$name>"))
      } else {
        val start: Doc = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">")
        val end: Doc = Doc.text(s"</$name>")

        val stackElements: Boolean = noText &&
          ((children.length >= 2) || ((children.length == 1) && alwaysStackElements.contains(label))) &&
          !doNotStackElements.contains(label)

        if (stackElements) {
          // If this is clearly a bunch of elements - stack 'em with an indent:
          Doc.cat(Seq(
            start,
            Doc.cat(children.map(child => (Doc.hardLine + child).nested(indent))),
            Doc.hardLine,
            end
          ))
        } else if (nestElements.contains(label)) {
          // If this is forced-nested element - nest it:
          Doc.intercalate(Doc.lineOrSpace, children).tightBracketBy(left = start, right = end, indent)
        } else {
          // Mixed content or non-break-off-able attachments on the side(s) cause flow-style;
          // character content should stick to the opening and closing tags:
          Doc.cat(Seq(
            start,
            if (canBreakLeft && !charactersLeft) Doc.lineOrEmpty else Doc.empty,
            Doc.intercalate(Doc.lineOrSpace, children),
            if (canBreakRight && !charactersRight) Doc.lineOrEmpty else Doc.empty,
            end
          ))
        }
      }
    }

    private def getName(element: model.Element): String =
      model.getPrefix(element).fold("")(_ + ":") + model.getName(element)

    private def getAttributeValues(element: model.Element, parent: Option[model.Element]): Seq[Attribute.Value[String]] = {
      val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(model.getNamespaces)
      model.getNamespaces(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
      model.getAttributes(element).filterNot(_.value.isEmpty)
    }

    @scala.annotation.tailrec
    private def atomize(result: model.Nodes, nodes: model.Nodes): model.Nodes =
      if (nodes.isEmpty) result else {
        val (texts: model.Nodes, tail: model.Nodes) = nodes.span(model.isText)

        val newResult: model.Nodes = if (texts.isEmpty) result else result ++
          processText(Seq.empty, texts.head,
            Strings.squashBigWhitespace(texts.map(model.asText).map(model.getText).mkString("")))

        if (tail.isEmpty) newResult
        else atomize(newResult :+ tail.head, tail.tail)
      }

    @scala.annotation.tailrec
    private def processText(result: Seq[model.Text], seed: model.Node, text: String): Seq[model.Text] =
      if (text.isEmpty) result else {
        val (spaces: String, tail: String) = text.span(_ == ' ')
        val newResult = if (spaces.isEmpty) result else result :+ model.mkText(" ", seed)
        val (word: String, tail2: String) = tail.span(_ != ' ')

        if (word.isEmpty) newResult
        else processText(newResult :+ model.mkText(word, seed), seed, tail2)
      }

    @scala.annotation.tailrec
    private def chunkify(
      result: Seq[model.Nodes],
      current: model.Nodes,
      nodes: model.Nodes,
      flush: Boolean
    ): Seq[model.Nodes] =
      if (flush) chunkify(result :+ current.reverse, Nil, nodes, flush = false) else (current, nodes) match {
        case (Nil    , Nil    ) => result
        case (_      , Nil    )                                           => chunkify(result, current     , Nil     , flush = true )
        case (Nil    , n :: ns) if  model.isWhitespace(n)                 => chunkify(result, Nil         , ns      , flush = false)
        case (_      , n :: ns) if  model.isWhitespace(n)                 => chunkify(result, current     , ns      , flush = true )
        case (Nil    , n :: ns) if !model.isWhitespace(n)                 => chunkify(result, n :: Nil    , ns      , flush = false)
        case (c :: cs, n :: ns) if !model.isWhitespace(n) &&  cling(c, n) => chunkify(result, n :: c :: cs, ns      , flush = false)
        case (c :: _ , n :: ns) if !model.isWhitespace(n) && !cling(c, n) => chunkify(result, current     , n :: ns , flush = true )
      }

    private def cling(c: model.Node, n: model.Node): Boolean = model.isText(c) || model.isText(n) ||
      (model.isElement(n) && clingyElements.contains(model.getName(model.asElement(n))))

    private def fromChunk(
      nodes: model.Nodes,
      parent: Option[model.Element],
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

    private def fromPreformattedNode(node: model.Node, parent: Option[model.Element]): Seq[String] =
      if (model.isElement(node)) fromPreformattedElement(model.asElement(node), parent)
      else if (model.isText(node)) preformattedLines(model.getText(model.asText(node)))
      else preformattedLines(model.toString(node))

    private def fromNode(
      node: model.Node,
      parent: Option[model.Element],
      canBreakLeft: Boolean,
      canBreakRight: Boolean
    ): Doc =
      if (model.isElement(node)) {
        val element: model.Element = model.asElement(node)
        if (preformattedElements.contains(model.getName(element)))
          Doc.text(fromPreformattedElement(element, parent).mkString(PrettyPrinter.hiddenNewline))
        else {
          val result: Doc = fromElement(element, parent, canBreakLeft, canBreakRight)
          // Note: suppressing extra hardLine when lb is in stack is non-trivial - and not worth it :)
          if (canBreakRight && model.getName(element) == "lb") result + Doc.hardLine else result
        }
      }
      else if (model.isText(node)) Doc.text(encodeXmlSpecials(model.getText(model.asText(node))))
      else Doc.paragraph(model.toString(node))
  }

  private def preformattedLines(string: String): Seq[String] =
    encodeXmlSpecials(string).split("\n").toSeq

  // Since I now encode XML specials when pretty-printing, there is no need for the elaborate double-escaping
  // in the Markdown code-blocks, so DoubleEscapeHtmlRendererExtension was removed.
  private def encodeXmlSpecials(string: String): String =
    if (encodeXmlSpecials) Strings.encodeXmlSpecials(string) else string
}

object PrettyPrinter {

  trait Recognizer {
    def recognize(model: Model)(element: model.Element): (PrettyPrinter, Option[Doctype])
  }

  // The only way I found to not let Paiges screw up indentation in the <pre><code>..</code></pre> blocks
  // is to give it the whole block as one unbreakable text, and for that I need to hide newlines from it -
  // and then restore them in render()...
  // Also, element start and end tags must not be separated from the children by newlines...
  val hiddenNewline: String = "\\n"

  val default: PrettyPrinter = new PrettyPrinter

  def prettyPrint(roots: List[File], model: Model, recognizer: Recognizer): Unit = {
    val (directories: List[File], files: List[File]) = roots.partition(_.isDirectory)
    val xmlFiles: List[File] = files.filter(isXml) ++ listXmlFiles(List.empty, directories)
    xmlFiles.foreach { file =>
      // Note: Scala XML ignores XML comments when parsing a file
      // (see https://github.com/scala/scala-xml/issues/508);
      // using Dom instead; use Dom to keep them...
      val element: model.Element = model.loadFromUrl(Files.file2url(file))
      val (prettyPrinter, doctype) = recognizer.recognize(model)(element)
      Files.write(
        file = file,
        content = prettyPrinter.render(model)(element, addXmlHeader = true, doctype)
      )
    }
  }

  @scala.annotation.tailrec
  def listXmlFiles(result: List[File], directoriesToList: List[File]): List[File] = directoriesToList match {
    case Nil => result
    case current :: tail =>
      val (directories: List[File], files: List[File]) = current.listFiles.toList.partition(_.isDirectory)
      listXmlFiles(result ++ files.filter(isXml), tail ++ directories)
  }

  private def isXml(file: File): Boolean = Files.nameAndExtension(file.getName)._2.contains("xml")
}
