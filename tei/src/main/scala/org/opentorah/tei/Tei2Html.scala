package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Namespace, Xhtml, Xml}
import scala.xml.{Elem, Node}

// TODO introduce XXX -> Html converters (start with TEI):
// - applies only to elements in a specified namespace
// - prefixes the names of attributes that clash with HTML
// - convert xml:id to id; if both are present - error;
// - applies transformation that takes care of the linking and table elements

// TODO
// - split resolving from Tei->Html transform, so that Markdown can be transformed to Html and still resolved,
//   - move resolver out of TEI (into Html)
// - split notes handling into a separate pass; the rest of TEI -> HTML does not need state...
// TODO
// - copy xml:id to id (why?); what is the difference between xml:id and id?
// - copy xml:lang to lang (why?);
// - copy rendition to class, removing the (leading?) '#' (or just use 'rendition' - TEI defines its own 'class'?);
// - transform tagsDecl?
// - transform prefixDef?
object Tei2Html {

  private final class EndNote(val number: Int, val id: Option[String], val content: Seq[Node]) {
    val contentId: String = s"_note_$number"
    val srcId: String = id.getOrElse(s"src_note_$number")
  }

  private final class State(val resolver: TeiResolver, val notes: Seq[EndNote])

  def transform(resolver: TeiResolver, element: Elem): Elem = {
    def t(element: Elem): (Elem, State) = Xml.transform[State](
      element,
      new State(resolver, Seq.empty),
      elementTransformer
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

  private val classAttribute: Attribute[String] = Attribute("class")
  private val targetAttribute: Attribute[String] = Attribute("target")
  private val urlAttribute: Attribute[String] = Attribute("url")
  private val placeAttribute: Attribute[String] = Attribute("place")
  private val colsAttribute: Attribute[String] = Attribute("cols")

  private val teiAttributesSpecialForHtml: Set[Attribute[String]] = Set(
    targetAttribute
  )

  private def addAttributes(
    element: Elem,
    htmlElement: Elem,
    excludeAttributes: Seq[Attribute[String]],
  ): Elem = Attribute.setAll(
    // TODO
    //  - drop 'target' attribute
    //  - change xml:id attribute to just id
    //  - add such a method to Html
    // TODO do not copy to htmlElement attributes already set on it,
    //  eliminate the excludeAttributes parameter
    //  and add such a method to Attribute (addAll()?)
    Xhtml.namespace.default.declare(htmlElement),
    Seq(classAttribute.withValue(element.label)) ++
      Attribute.getAll(htmlElement) ++
      Attribute.getAll(element)
        .filterNot(attributeWithValue => teiAttributesSpecialForHtml.contains(attributeWithValue.attribute))
        .filterNot(attributeWithValue => excludeAttributes.contains(attributeWithValue.attribute))
  )

  private def elementTransformer(element: Elem, state: State): (Elem, State) = {
    def toHtml(
      htmlElement: Elem,
      excludeAttributes: Seq[Attribute[String]] = Seq.empty,
      newState: State = state,
      isEmpty: Boolean = false
    ): (Elem, State) = {
      val withAttributes: Elem = addAttributes(element, htmlElement, excludeAttributes)

      val result: Elem =
        if (isEmpty) {
          require(Xml.isEmpty(element))
          withAttributes
        } else withAttributes.copy(child = Xml.getChildren(element))

      (result, newState)
    }

    val result: (Elem, State) =
      if (Namespace.get(element) != Tei.namespace.default) (element, state)
      else element.label match {
        case "head" =>
          (element.copy(label = "tei-head"), state)

        case "body" =>
          (element.copy(label = "tei-body"), state)

        case label if EntityType.isName(label) =>
          require(!Xml.isEmpty(element), element)
          val ref: Option[String] = EntityName.refAttribute.get(element)
          val (href: Option[String], role: Option[String]) =
            ref.flatMap(ref => state.resolver.findByRef(ref)).map(resolved => (
              Some(Files.mkUrl(resolved.url)),
              resolved.role
            )).getOrElse((ref, None))

          toHtml(<a href={href.orNull} target={role.orNull}/>)

        case Ref.elementName =>
          require(!Xml.isEmpty(element))
          val target: String = targetAttribute.doGet(element)
          val (href: String, role: Option[String]) =
            state.resolver.resolve(target).map(resolved => (
              Files.mkUrl(resolved.url),
              resolved.role
            )).getOrElse((target, None))

          toHtml(
            <a href={href} target={role.orNull}/>
          )

        case "ptr" =>
          // TODO do not add text (href) to the ptr
          val href: String = targetAttribute.doGet(element)
          toHtml(
            <a href={href}>{href}</a>,
            isEmpty = true
          )

        case Pb.elementName =>
          // TODO make a separate pass adding ids to pbs, name references etc. - ensuring their uniqueness
          val pageId: String = Page.pageId(Pb.nAttribute.doGet(element))
          toHtml(
            <a xml:id={pageId}
               href={Files.mkUrl(Files.addPart(state.resolver.facs.url, pageId))}
               target={state.resolver.facs.role.orNull}>⎙</a>,
            excludeAttributes = Seq(Xml.idAttribute),
            isEmpty = true
          )

        case "graphic" =>
          // TODO In TEI <graphic> can contain <desc>, but are treating it as empty.
          toHtml(
            <img src={urlAttribute.doGet(element)}/>,
            isEmpty = true
          )

        case "table" =>
          toHtml(<table/>)

        // TODO before the first row there can be <head>HEAD</head>; it becomes <caption>transform(HEAD)</caption>...
        case "row" =>
          toHtml(<tr/>)

        case "cell" =>
          toHtml(<td colspan={colsAttribute.get(element).orNull}/>)

        case "note" if placeAttribute.get(element).contains("end") =>
          val note: EndNote = new EndNote(
            number = state.notes.length + 1,
            id = Xml.idAttribute.get(element),
            content = Xml.getChildren(element)
          )

          (
            addAttributes(
              element,
              <a id={note.srcId} href={s"#${note.contentId}"}><sup>{note.number}</sup></a>,
              excludeAttributes = Seq(Xml.idAttribute)
            ),
            new State(state.resolver, state.notes :+ note)
          )

        case _ => (element, state)
      }

    result
  }
}
