package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.xml.Xml.StateTransformer
import org.opentorah.xml.{Attribute, LinkResolver, Xhtml, Xml}
import scala.xml.{Elem, Node}

// TODO split into:
// - numbering: add xml:id on pbs, name references etc. - ensuring their uniqueness
// - transform (Element -> Element)
// - link resolution (so that Markdown can be transformed to Html and resolved)
// - notes handling into a separate pass.

// TODO
// - copy rendition to class, removing the (leading?) '#' (or just use 'rendition')?;
// - transform tagsDecl?
// - transform prefixDef?
object Tei2Html {

  private final class EndNote(val number: Int, val id: Option[String], val content: Seq[Node]) {
    val contentId: String = s"_note_$number"
    val srcId: String = id.getOrElse(s"src_note_$number")
  }

  private final class State(val resolver: LinkResolver, val notes: Seq[EndNote])

  def transform(resolver: LinkResolver, element: Elem): Elem = {
    val namespaceTransformer: StateTransformer[State] = Xhtml.lift[State](elementTransformer, Tei.namespace)

    def t(element: Elem): (Elem, State) = Xml.transform[State](
      element,
      new State(resolver, Seq.empty),
      namespaceTransformer
    )

    val (result, finalState) = t(element)
    <div xmlns={Xhtml.namespace.uri} class="html">
      {result}
      <div xmlns={Xhtml.namespace.uri} class="endnotes">{
        for (note <- finalState.notes) yield t(
          <span xmlns={Xhtml.namespace.uri} class="endnote" id={note.contentId}>
            <a class="endnote-backlink" href={s"#${note.srcId}"}>{note.number}</a>
            {note.content}
          </span>
        )/* TODO do not ignore end-notes inside end-notes */._1
        }</div>
    </div>
  }

  private val targetAttribute: Attribute[String] = Attribute("target")
  private val urlAttribute: Attribute[String] = Attribute("url")
  private val placeAttribute: Attribute[String] = Attribute("place")
  private val colsAttribute: Attribute[String] = Attribute("cols")

  private def elementTransformer(element: Elem, state: State): (Elem, State) = {
    val result: (Elem, State) = element.label match {
      case label if EntityType.isName(label) =>
        require(!Xml.isEmpty(element), element)
        val ref: Option[String] = EntityName.refAttribute.get(element)
        val (href: Option[String], role: Option[String]) =
          ref.flatMap(ref => state.resolver.findByRef(ref)).map(resolved => (
            Some(Files.mkUrl(resolved.url)),
            resolved.role
          )).getOrElse((ref, None))

        (<a href={href.orNull} target={role.orNull}>{Xml.getChildren(element)}</a>, state)

      case Ref.elementName =>
        require(!Xml.isEmpty(element))
        val target: String = targetAttribute.doGet(element)
        val (href: String, role: Option[String]) =
          state.resolver.resolve(target).map(resolved => (
            Files.mkUrl(resolved.url),
            resolved.role
          )).getOrElse((target, None))

        (<a href={href} target={role.orNull}>{Xml.getChildren(element)}</a>, state)

      case "ptr" =>
        require(Xml.isEmpty(element))
        (<a href={targetAttribute.doGet(element)}/>, state)

      case Pb.elementName =>
        require(Xml.isEmpty(element))
        val pageId: String = Page.pageId(Pb.nAttribute.doGet(element))
        (
          <a id={pageId}
             href={Files.mkUrl(Files.addPart(state.resolver.facs.url, pageId))}
             target={state.resolver.facs.role.orNull}>⎙</a>,
          state
        )

      case "graphic" =>
        require(Xml.isEmpty(element))
        // Note: in TEI <graphic> can contain <desc>, but are treating it as empty.
        (<img src={urlAttribute.doGet(element)}/>, state)

      case "table" =>
        (<table>{Xml.getChildren(element)}</table>, state)

      // Note: before the first row there can be <head>HEAD</head>; it should become <caption>transform(HEAD)</caption>.
      case "row" =>
        (<tr>{Xml.getChildren(element)}</tr>, state)

      case "cell" =>
        (<td colspan={colsAttribute.get(element).orNull}>{Xml.getChildren(element)}</td>, state)

      case "note" if placeAttribute.get(element).contains("end") =>
        val note: EndNote = new EndNote(
          number = state.notes.length + 1,
          id = Xml.idAttribute.get(element),
          content = Xml.getChildren(element)
        )

        (
          <a id={note.srcId} href={s"#${note.contentId}"}><sup>{note.number}</sup></a>,
          new State(state.resolver, state.notes :+ note)
        )

      case _ => (element, state)
    }

    result
  }
}
