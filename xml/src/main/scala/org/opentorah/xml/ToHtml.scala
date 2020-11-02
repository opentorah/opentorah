package org.opentorah.xml

import org.opentorah.util.Files
import zio.{URIO, ZIO}
import scala.xml.Node

trait ToHtml {

  protected def namespace: Namespace

  protected def elementTransform(element: Xml.Element): URIO[State, Xml.Element]

  protected def isEndNote(element: Xml.Element): Boolean

  final protected def link(
    ref: Option[String],
    r: (State, String) => Option[LinkResolver.Resolved],
    children: Seq[Node]
  ): URIO[State, Xml.Element] = for {
    resolved <- ref.map(ref => ZIO.access[State](r(_, ref))).getOrElse(ZIO.none)
  } yield {
    val href: Option[String] = resolved.map(resolved => Files.mkUrl(resolved.url)).orElse(ref)
    val role: Option[String] = resolved.flatMap(_.role)
    <a href={href.orNull} target={role.orNull}>{children}</a>
  }

  final class EndNote(val number: Int, val id: Option[String], val content: Seq[Node]) {
    val contentId: String = s"_note_$number"
    val srcId: String = id.getOrElse(s"src_note_$number")
  }

  // TODO add pre-existing ids as a set and take it into account when getting a new id (including for notes)
  // TODO add state for nested section ids
  final class State(val resolver: LinkResolver) {
    private var endNotes: Seq[EndNote] = Seq.empty

    // TODO get two ids, one for the actual content at the end
    def addEndnote(id: Option[String], content: Seq[Node]): EndNote = {
      val result = new EndNote(
        number = endNotes.length + 1,
        id,
        content
      )
      endNotes = endNotes :+ result
      result
    }

    def getEndNotes: Seq[EndNote] = endNotes
  }

  def toHtml(resolver: LinkResolver, element: Xml.Element): Xml.Element = {
    val (result, finalState) = runTransform(element, resolver)
    <div xmlns={Xhtml.namespace.uri} class="html">
      {result}
      {runTransform(<div xmlns={Xhtml.namespace.uri} class="endnotes">{
        for (note <- finalState.getEndNotes) yield
          <span xmlns={Xhtml.namespace.uri} class="endnote" id={note.contentId}>
            <a class="endnote-backlink" href={s"#${note.srcId}"}>{note.number}</a>
            {note.content}
          </span>
        }</div>, resolver)/* TODO do not ignore end-notes inside end-notes */._1}
    </div>
  }

  private def runTransform(element: Xml.Element, resolver: LinkResolver): (Xml.Element, State) = Xml.runTransform(
    element,
    new State(resolver),
    Xml.inNamespace[State](namespace, liftedElementTransform)
  )

  private val htmlElementNames: Set[String] = Set("head", "body")

  private def liftedElementTransform(element: Xml.Element): URIO[State, Xml.Element] = for {
    newElement <- elementTransform(element)
  } yield {
    val name: String = element.label
    val attributes: Seq[Attribute.Value[String]] = Attribute.getAll(element)
    if (newElement eq element) Attribute.setAll(
      element = if (htmlElementNames.contains(name)) element.copy(label = addPrefix(name)) else element,
      attributes = attributes.map(transformXmlAttribute)
    ) else Attribute.addAll(
      element = Xhtml.namespace.default.declare(newElement),
      attributes = Xhtml.classAttribute.withValue(name) +: attributes.map(transformAttribute)
    )
  }

  private val htmlAttributeNames: Set[String] = Set("class", "target", "href")

  private def transformAttribute(attribute: Attribute.Value[String]): Attribute.Value[String] = {
    val result: Attribute.Value[String] = transformXmlAttribute(attribute)
    val name: String = result.attribute.name
    if (htmlAttributeNames.contains(name))
      Attribute(addPrefix(name)).withOptionalValue(result.value)
    else
      result
  }

  private def transformXmlAttribute(attribute: Attribute.Value[String]): Attribute.Value[String] =
    if (attribute.attribute == Xml.idAttribute  ) Xhtml.idAttribute  .withOptionalValue(attribute.value) else
    if (attribute.attribute == Xml.langAttribute) Xhtml.langAttribute.withOptionalValue(attribute.value) else
      attribute

  private def addPrefix(name: String): String = s"${namespace.getPrefix.get}-$name"
}
