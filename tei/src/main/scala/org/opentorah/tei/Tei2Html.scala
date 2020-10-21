package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Namespace, Xhtml, Xml}
import scala.xml.{Elem, Node}

// TODO when transforming <TEI>, <titleStmt><title>TITLE</title></titleStmt> should become <html><head><title>TITLE</title></head></html>
// TODO copy attributes:
// - xml:id to id (why?);
// - xml:lang to lang (why?);
// - rendition to class, removing the (leading?) '#' (or just use 'rendition' - TEI defines its own 'class'?);
// TODO:
// - transform tagsDecl?
// - transform prefixDef?
object Tei2Html {

  case class TransformResult[S](
    htmlElement: Elem,
    excludeAttributes: Seq[Attribute[String]],
    state: S
  )

  type Transformer[S] = (Elem, S) => Option[TransformResult[S]]

  private val classAttribute: Attribute[String] = Attribute("class")

  private val specialElements: Set[String] = Set("head", "body")

  private def transformElement[S](element: Elem, state: S, transformer: Transformer[S]): (Elem, S) = {
    val (newElement, newState) = if (Namespace.get(element) != Tei.namespace.default) (element, state) else {
      if (specialElements.contains(element.label)) (element.copy(label = s"tei-${element.label}"), state) else {
        transformer(element, state).fold((element, state)) { result: TransformResult[S] =>
          val htmlElement = Attribute.setAll(
            Xhtml.namespace.default.declare(result.htmlElement),
            Seq(classAttribute.withValue(element.label)) ++
              Attribute.getAll(result.htmlElement) ++
              Attribute.getAll(element).filterNot(attributeWithValue => result.excludeAttributes.contains(attributeWithValue.attribute))
          )

          (htmlElement, result.state)
        }
      }
    }

    val (children, finalState) =
      transformNodes[S](Seq.empty, Xml.getChildren(newElement), newState, transformer)

    (newElement.copy(child = children), finalState)
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

  private val refAttribute: Attribute[String] = Attribute("ref")
  private val targetAttribute: Attribute[String] = Attribute("target")
  private val urlAttribute: Attribute[String] = Attribute("url")
  private val placeAttribute: Attribute[String] = Attribute("place")
  private val colsAttribute: Attribute[String] = Attribute("cols")

  private def elementTransformer(element: Elem, state: State): Option[TransformResult[State]] = element.label match {

    case label if EntityType.isName(label) =>
      for {
        ref <- refAttribute.get(element)
        resolved <- state.resolver.findByRef(ref)
      } yield TransformResult(
        <a href={Files.mkUrl(resolved.url)} target={resolved.role.orNull}>{Xml.getChildren(element)}</a>,
        Seq.empty,
        state
      )

      // TODO clean up the duplication
    case Ref.elementName =>
      require(!Xml.isEmpty(element))
      val target = targetAttribute.doGet(element)
      if (!target.startsWith("/")) {
        Some(TransformResult(
          <a href={target}>
            {Xml.getChildren(element)}
          </a>,
          Seq(targetAttribute),
          state
        ))
      } else {
        val (url, part) = Files.urlAndPart(target)
        state.resolver.resolve(url).map { resolved =>
          TransformResult(
            <a href={Files.mkUrl(Files.addPart(resolved.url, part))} target={resolved.role.orNull}>
              {Xml.getChildren(element)}
            </a>,
            Seq(targetAttribute),
            state
          )
        }
      }

    case Pb.elementName =>
      val pageId: String = Page.pageId(Pb.nAttribute.doGet(element))
      require(Xml.isEmpty(element))
      Some(TransformResult(
        <a xml:id={pageId}
           href={Files.mkUrl(Files.addPart(state.resolver.facs.url, pageId))}
           target={state.resolver.facs.role.orNull}>⎙</a>,
        Seq(Xml.idAttribute),
        state
      ))

    case "ptr" =>
      val href = targetAttribute.doGet(element)
      Some(TransformResult(
        <a href={href}>{href}</a>,
        Seq(targetAttribute),
        state
      ))

    case "graphic" =>
      // TODO In TEI <graphic> can contain <desc>, but are treating it as empty.
      require(Xml.isEmpty(element))
      Some(TransformResult(
        <img src={urlAttribute.doGet(element)}/>,
        Seq.empty,
        state
      ))

    case "supplied" =>
      Some(TransformResult(
        <span>{Xml.mkText("[") +: Xml.getChildren(element) :+ Xml.mkText("]")}</span>,
        Seq.empty,
        state
      ))

    case "table" =>
      Some(TransformResult(
        <table>{Xml.getChildren(element)}</table>,
        Seq.empty,
        state
      ))

    // TODO before the first row there can be <head>HEAD</head>; it becomes <caption>transform(HEAD)</caption>...
    case "row" =>
      Some(TransformResult(
        <tr>{Xml.getChildren(element)}</tr>,
        Seq.empty,
        state
      ))

    case "cell" =>
      Some(TransformResult(
        <td colspan={colsAttribute.get(element).orNull}>{Xml.getChildren(element)}</td>,
        Seq.empty, // TODO Seq(colsAttribute)?
        state
      ))

    case "note" if placeAttribute.get(element).contains("end") =>
      val note: EndNote = new EndNote(
        number = state.notes.length + 1,
        id = Xml.idAttribute.get(element),
        content = Xml.getChildren(element)
      )

      Some(TransformResult(
        <a id={note.srcId} href={s"#${note.contentId}"}><sup>{note.number}</sup></a>,
        Seq(Xml.idAttribute),
        new State(state.resolver, state.notes :+ note)
      ))

    case _ => None
  }

  private final class EndNote(val number: Int, val id: Option[String], val content: Seq[Node]) {
    val contentId: String = s"_note_$number"
    val srcId: String = id.getOrElse(s"src_note_$number")
  }

  private final class State(val resolver: TeiResolver, val notes: Seq[EndNote])

  def transform(resolver: TeiResolver, element: Elem): Elem = {
    def t(element: Elem): (Elem, State) = transformElement[State](
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
            <a class="endnote-backlink" href={s"#${note.srcId}"}>{note.number}.</a>
            {note.content}
          </span>
        )/* TODO do not ignore end-notes inside end-notes */._1
      }</div>
    </div>
  }
}
