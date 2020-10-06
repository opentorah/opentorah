package org.opentorah.tei

import org.opentorah.xml.{Attribute, Namespace, Xhtml, Xml}
import scala.xml.{Elem, Node}

// TODO monodize/zioify threadding of EndNotes through the transformation?
// TODO remove spurious XML namespace?
// TODO do I need to re-base relative URLs in links and images?
// TODO copy attributes:
// - xml:id to id (why?);
// - xml:lang to lang (why?);
// - rendition to class, removing the (leading?) '#' (or just use 'rendition' - TEI defines its own 'class'?);
// TODO:
// - transform tagsDecl?
// - transform prefixDef?
// TODO when transforming <TEI>, <titleStmt><title>TITLE</title></titleStmt> should become <html><head><title>TITLE</title></head></html>
object Tei2Html {

  case class TransformResult[S](
    htmlElement: Elem,
    excludeAttributes: Seq[Attribute[String]],
    state: S
  )

  type Transformer[S] = PartialFunction[(Elem, S), TransformResult[S]]

  private val classAttribute: Attribute[String] = Attribute("class")

  private def transformElement[S](element: Elem, state: S, transformer: Transformer[S]): (Elem, S) = {
    val result: Option[TransformResult[S]] =
      if (Namespace.get(element) != Tei.namespace.default) None else transformer.lift(element, state)

    val (newElement, newState) = result.fold((element, state)) { result: TransformResult[S] =>
      val htmlElement = Attribute.setAll(
        Xhtml.namespace.default.declare(result.htmlElement),
        Seq(classAttribute.withValue(element.label)) ++
        Attribute.getAll(result.htmlElement) ++
        Attribute.getAll(element).filterNot(attributeWithValue => result.excludeAttributes.contains(attributeWithValue.attribute))
      )

      (htmlElement, result.state)
    }

    val (children, finalState) =
      transformNodes[S](Seq.empty, Xml.getChildren(newElement), newState, transformer)

    (element.copy(child = children), finalState)
  }

  @scala.annotation.tailrec
  private def transformNodes[S](
    result: Seq[Node],
    nodes: Seq[Node],
    state: S,
    transformer: Transformer[S]
  ): (Seq[Node], S) = nodes match {
    case Seq() => (result, state)
    case Seq(n, ns @ _*) =>
      val (nTransformed, nextState) =
        if (!Xml.isElement(n)) (n, state)
        else transformElement(Xml.asElement(n), state, transformer)
      transformNodes(result :+ nTransformed, ns, nextState, transformer)
  }

  private val targetAttribute: Attribute[String] = Attribute("target")
  private val roleAttribute: Attribute[String] = Attribute("role")
  private val urlAttribute: Attribute[String] = Attribute("url")
  private val placeAttribute: Attribute[String] = Attribute("place")
  private val colsAttribute: Attribute[String] = Attribute("cols")

  private val elementTransformer: Transformer[State] = {
    case (element, state) if EntityType.isName(element.label) || (element.label == Ref.elementName) =>
      a(element, None, state)

    case (element, state) if element.label == Pb.elementName =>
      a(element, Some("⎙"), state)

    case (element, state) if element.label == "ptr" =>
      a(element, Some(targetAttribute.doGet(element)), state)

    case (element, state) if element.label == "graphic" =>
      // TODO In TEI <graphic> can contain <desc>, but are treating it as empty.
      require(Xml.getChildren(element).isEmpty)
      TransformResult(
        <img src={urlAttribute.doGet(element)}/>,
        Seq.empty,
        state
      )

    case (element, state) if element.label == "supplied" =>
      TransformResult(
        <ab>{Xml.mkText("[") +: Xml.getChildren(element) :+ Xml.mkText("]")}</ab>,
        Seq.empty,
        state
      )

    case (element, state) if element.label == "table" =>
      TransformResult(
        <table>{Xml.getChildren(element)}</table>,
        Seq.empty,
        state
      )

    // TODO before the first row there can be <head>HEAD</head>; it becomes <caption>transform(HEAD)</caption>...
    case (element, state) if element.label == "row" =>
      TransformResult(
        <tr>{Xml.getChildren(element)}</tr>,
        Seq.empty,
        state
      )

    case (element, state) if element.label == "cell" =>
      TransformResult(
        <td colspan={colsAttribute.get(element).orNull}>{Xml.getChildren(element)}</td>,
        Seq(colsAttribute),
        state
      )

    case (element, state) if element.label == "note" && placeAttribute.get(element).contains("end") =>
      val note: EndNote = new EndNote(
        number = state.notes.length + 1,
        id = Xml.idAttribute.get(element),
        content = Xml.getChildren(element)
      )

      TransformResult(
        <a id={note.srcId} href={s"#${note.contentId}"}><sup>{note.number}</sup></a>,
        Seq(Xml.idAttribute),
        new State(state.resolver, state.notes :+ note)
      )
  }

  private def a(
    element: Elem,
    content: Option[String],
    state: State
  ): TransformResult[State] = TransformResult(
    <a href={targetAttribute.get(element).orNull} target={roleAttribute.get(element).orNull}>
      {content.fold(Xml.getChildren(element)) { content =>
        require(Xml.getChildren(element).isEmpty)
        Seq(Xml.mkText(content))
      }}
    </a>,
    Seq(targetAttribute, roleAttribute),
    state
  )

  // TODO
  //
  //  pb:
  //   pageId: String = Page.pageId(nAttribute.doGet(elem))
  //   xml:id={pageId}
  //   target = resolver.facs.role.orNull
  //   href = Files.mkUrl(Files.addPart(resolver.facs.url, pageId))
  //
  //  entityName:
  //
//  def transformer(resolver: TeiResolver): Xml.Transformer = elem =>
//    if (!EntityType.isName(elem.label)) elem else {
//      refAttribute.get(elem).fold(elem) { ref =>
//        resolver.findByRef(ref).fold(elem) { resolved =>
//          Attribute.setAll(elem, Seq(
//            roleAttribute.withValue(resolved.role.orNull),
//            targetAttribute.withValue(Files.mkUrl(resolved.url))
//          ))
//        }
//      }
//    }
//
//  def transformer(resolver: TeiResolver): Xml.Transformer = elem => if (elem.label != elementName) elem else {
//    if (elem.child.forall(Xml.isWhitespace)) println(s"No reference text: $elem")
//    targetAttribute.get(elem).fold(throw new IllegalArgumentException(s"empty target: $elem")) { target =>
//      if (!target.startsWith("/")) elem else {
//        val (url, part) = Files.urlAndPart(target)
//        resolver.resolve(url).fold(elem) { resolved =>
//          val role: Option[String] = resolved.role
//          if (role.isEmpty) elem else {
//            Attribute.setAll(elem, Seq(
//              roleAttribute.withValue(role),
//              targetAttribute.withValue(Files.mkUrl(Files.addPart(resolved.url, part))),
//              renditionAttribute.withValue(renditionAttribute.get(elem))
//            ))
//          }
//        }
//      }
//    }
//  }

  private final class EndNote(val number: Int, val id: Option[String], val content: Seq[Node]) {
    val contentId: String = s"_note_$number"
    val srcId: String = id.getOrElse(s"src_note_$number")
  }

  // TODO switch away from ol/li to div/span/etc., with numbers (red) backlinks, and formatted in
  // in sequence with padding between the notes.
  private final class State(val resolver: TeiResolver, val notes: Seq[EndNote])

  def transform(resolver: TeiResolver): Xml.Transformer = element => {
    def t(element: Elem): (Elem, State) = transformElement[State](
      element,
      new State(resolver, Seq.empty),
      elementTransformer
    )

    val (result, finalState) = t(element)
    <div xmlns={Xhtml.namespace.uri} class="html">
      {result}
      <ol xmlns={Xhtml.namespace.uri} class="endnotes">{
        for (note <- finalState.notes) yield t(
          <li xmlns={Xhtml.namespace.uri} class="endnote" id={note.contentId}>
            {note.content}
            <a class="endnote-backlink" href={s"#${note.srcId}"}>{s"[${note.number}]"}</a>
          </li>
        )/* Ignore end-notes inside end-notes */._1
      }</ol>
    </div>
  }
}
