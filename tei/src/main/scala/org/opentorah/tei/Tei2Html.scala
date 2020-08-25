package org.opentorah.tei

import org.opentorah.xml.{Attribute, Namespace, Xhtml, Xml}
import scala.xml.{Elem, Node}

// TODO monodize/ziofy threadding of EndNotes through the transformation!
// TODO remove spurious XML namespace?
// TODO do I need to re-base relative URLs in links and images?
// TODO append endnotes container to the end.
// TODO copy attributes:
// - xml:id to id (why?);
// - xml:lang to lang (why?);
// - rendition to class, removing the (leading?) '#' (or just use 'rendition' - TEI defines its own 'class'?);
// TODO:
// - transform tagsDecl?
// - transform prefixDef?
// TODO when transforming <TEI>, <titleStmt><title>TITLE</title></titleStmt> should become <html><head><title>TITLE</title></head></html>
object Tei2Html {

  private val classAttribute: Attribute[String] = Attribute("class")
  private val targetAttribute: Attribute[String] = Attribute("target")
  private val roleAttribute: Attribute[String] = Attribute("role")
  private val urlAttribute: Attribute[String] = Attribute("url")
  private val placeAttribute: Attribute[String] = Attribute("place")
  private val colsAttribute: Attribute[String] = Attribute("cols")

  def transformNodes(nodes: Seq[Node], endNotes: EndNotes): (Seq[Node], EndNotes) =
    transformNodes(Seq.empty, nodes, endNotes)

  @scala.annotation.tailrec
  private def transformNodes(result: Seq[Node], nodes: Seq[Node], endNotes: EndNotes): (Seq[Node], EndNotes) =
    nodes match {
      case Seq() => (result, endNotes)
      case Seq(n, ns @ _*) =>
        val (nTransformed, nextEndNotes) = transformNode(n, endNotes)
        transformNodes(result :+ nTransformed, ns, nextEndNotes)
    }

  private def transformNode(node: Node, endNotes: EndNotes): (Node, EndNotes) =
    if (!Xml.isElement(node)) (node, endNotes)
    else transformElement(Xml.asElement(node), endNotes)

  def transformElement(element: Elem, endNotes: EndNotes): (Elem, EndNotes) = {
    if (Namespace.get(element) == Tei.namespace.default) transformTeiElement(element, endNotes)
    else transformChildren(element, endNotes)
  }

  private def transformChildren(element: Elem, endNotes: EndNotes): (Elem, EndNotes) = {
    val (children, nextEndNotes) = transformNodes(Xml.getChildren(element), endNotes)
    (element.copy(child = children), nextEndNotes)
  }

  private def transformTeiElement(element: Elem, endNotes: EndNotes): (Elem, EndNotes) = element.label match {
    case "ref" | "persName" | "orgName" | "placeName" =>
      link(element, endNotes)

    case "pb" =>
      link(element, endNotes, Some("⎙"))

    case "ptr" =>
      link(element, endNotes, targetAttribute.get(element))

    case "graphic" =>
      graphic(element, endNotes)

    case "supplied" =>
      val children: Seq[Node] = Xml.mkText("[") +: Xml.getChildren(element) :+ Xml.mkText("]")
      transformChildren(element.copy(child = children), endNotes)

    case "table" => transform(<table/>, element, None, endNotes)
    // TODO before the first row there can be <head>HEAD</head>; it becomes <caption>transform(HEAD)</caption>...
    case "row"   => transform(<tr/>, element, None, endNotes)
    case "cell"  => transform(<td colspan={colsAttribute.get(element).orNull}/>, element, None, endNotes, colsAttribute)

    case "note" if placeAttribute.get(element).contains("end") =>
      endNotes.addNote(element)

    case _ =>
      transformChildren(element, endNotes)
  }

  private def link(element: Elem, endNotes: EndNotes, text: Option[String] = None): (Elem, EndNotes) = {
    require(text.isEmpty || Xml.getChildren(element).isEmpty)

    transform(
      <a href={targetAttribute.get(element).orNull} target={roleAttribute.get(element).orNull}/>,
      element,
      text.map(value => Seq(Xml.mkText(value))),
      endNotes,
      targetAttribute,
      roleAttribute
    )
  }

  private def transform(
    htmlElement: Elem,
    teiElement: Elem,
    content: Option[Seq[Node]],
    endNotes: EndNotes,
    excludeAttributes: Attribute[String]*
  ): (Elem, EndNotes) = {
    val (newContent: Seq[Node], newEndNotes: EndNotes) =
      transformNodes(content.getOrElse(Xml.getChildren(teiElement)), endNotes)
    val result = setAttributes(htmlElement.copy(child = newContent), teiElement, excludeAttributes: _*)
    (result, newEndNotes)
  }

  private def graphic(element: Elem, endNotes: EndNotes): (Elem, EndNotes) = {
    // In TEI <graphic> can contain <desc>, but are treating it as empty.
    val result: Elem = setAttributes(<img src={urlAttribute.doGet(element)}/>, element)
    (result, endNotes)
  }

  private def setAttributes(htmlElement: Elem, teiElement: Elem, excludeAttributes: Attribute[String]*): Elem =
    Attribute.setAll(Xhtml.namespace.default.declare(htmlElement),
      Seq(classAttribute.withValue(teiElement.label)) ++
      Attribute.getAll(htmlElement) ++
      Attribute.getAll(teiElement)
      .filterNot(attributeWithValue => excludeAttributes.contains(attributeWithValue.attribute))
    )

  private final class EndNote(number: Int, id: Option[String], content: Seq[Node]) {
    private val contentId: String = s"_note_$number"
    private val srcId: String = id.getOrElse(s"src_note_$number")

    def linkXml: Elem = <a id={srcId} href={s"#$contentId"}><sup>{number}</sup></a>

    def contentXml: Elem =
      <li xmlns={Xhtml.namespace.uri} class="endnote" id={contentId}>
        {content}
        <a class="endnote-backlink" href={s"#$srcId"}>{s"[$number]"}</a>
      </li>
  }

  // TODO switch away from ol/li to div/span/etc., with numbers (red) backlinks, and formatted in
  // in sequence with padding between the notes.
  final class EndNotes(notes: Seq[EndNote]) {
    def contentXml: Elem =
      <ol xmlns={Xhtml.namespace.uri} class="endnotes">{for (note <- notes) yield note.contentXml}</ol>

    def addNote(element: Elem): (Elem, EndNotes) = {
      val content: Seq[Node] = Xml.getChildren(element)
      // Ignore end-notes inside end-notes:
      val note: EndNote = new EndNote(
        notes.length + 1,
        Xml.idAttribute.get(element),
        transformNodes(content, emptyEndNotes)._1
      )
      val result: Elem = setAttributes(note.linkXml, element, Xml.idAttribute)
      (result, new EndNotes(notes :+ note))
    }
  }

  val emptyEndNotes: EndNotes = new EndNotes(Seq.empty)
}
