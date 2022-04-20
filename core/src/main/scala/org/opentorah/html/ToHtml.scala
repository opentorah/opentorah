package org.opentorah.html

import org.opentorah.xml.{Attribute, Namespace, ScalaXml, Xml}
import zio.URIO

// Converting other XML dialects (e.g., TEI, DocBook) to HTML
trait ToHtml[R]:
  protected def namespace: Namespace

  protected def isFootnote(element: ScalaXml.Element): Boolean

  protected def isFootnotesContainer(element: ScalaXml.Element): Boolean = false

  protected def elementToHtml(
    element: ScalaXml.Element
  ): URIO[R, (ScalaXml.Element, ScalaXml.Nodes)]

  protected final def noTooltip(element: ScalaXml.Element): (ScalaXml.Element, ScalaXml.Nodes) =
    (element, Seq.empty)

  protected final def succeed(element: ScalaXml.Element): URIO[R, (ScalaXml.Element, ScalaXml.Nodes)] =
    URIO.succeed(noTooltip(element))

  final def toHtml(element: ScalaXml.Element): URIO[R, ScalaXml.Element] =
    processFootnotes(element).provideSomeLayer[R](Footnotes.empty).flatMap(elementsToHtml)

  private def processFootnotes(element: ScalaXml.Element): URIO[Footnotes & R, ScalaXml.Element] = for
    isEmpty: Boolean <- Footnotes.isEmpty
    doPush: Boolean = isEmpty || (isInNamespace(element) && isFootnotesContainer(element))
    _ <- if !doPush then URIO.succeed(()) else Footnotes.push
    newElement: ScalaXml.Element <-
      if isInNamespace(element) && isFootnote(element)
      then processFootnote(element)
      else transformChildren(processFootnotes, element)
    result: ScalaXml.Element <-
      if !doPush
      then URIO.succeed(newElement)
      else processLevels(newElement, Seq.empty)
  yield result

  private def processFootnote(element: ScalaXml.Element): URIO[Footnotes & R, ScalaXml.Element] =  for
    // TODO get two ids, one for the actual content at the end
    idNumber: Int <- Footnotes.takeNextIdNumber
    srcId: String = Xml.idAttribute.optional.get(ScalaXml)(element).getOrElse(s"footnote_src_$idNumber")
    contentId = s"footnote_$idNumber"
    number: Int <- Footnotes.getNextNumber
    symbol: String = number.toString
    _ <- Footnotes.add(
      <span xmlns={Html.namespace.uri} class="footnote" id={contentId}>
        <a href={s"#$srcId"} class="footnote-backlink">{symbol}</a>
        {ScalaXml.getChildren(element)}
      </span>
    )
  yield
    <a xmlns={Html.namespace.uri} href={s"#$contentId"} class="footnote-link" id={srcId}>{symbol}</a>

  private def processLevels(
    newElement: ScalaXml.Element,
    levels: Seq[Seq[ScalaXml.Element]]
  ): URIO[Footnotes & R, ScalaXml.Element] = for
    footnotes: Seq[ScalaXml.Element] <- Footnotes.get
    result: ScalaXml.Element <-
      if footnotes.isEmpty then for
        _ <- Footnotes.pop
      yield
        if levels.isEmpty then newElement else ScalaXml.setChildren(newElement,
          ScalaXml.getChildren(newElement) ++ <hr class="footnotes-line"/> ++
          (for level <- levels yield <div xmlns={Html.namespace.uri} class="footnotes">{level}</div>)
        )
      else for
        nextLevel: Seq[ScalaXml.Element] <- URIO.foreach(footnotes)(processFootnotes)
        result <- processLevels(newElement, levels :+ nextLevel)
      yield result
  yield result

  private def elementsToHtml(oldElement: ScalaXml.Element): URIO[R, ScalaXml.Element] = for
    element: ScalaXml.Element <- transformChildren(elementsToHtml, oldElement)
    result: ScalaXml.Element <- if !isInNamespace(element) then URIO.succeed(element) else
      val name: String = ScalaXml.getName(element)

      val attributes: Attribute.StringValues = ScalaXml.getAttributes(element).map((attribute: Attribute.Value[String]) =>
        ToHtml.xml2htmlAttribute.get(attribute.attribute).map(_.optional.withValue(attribute.value)).getOrElse {
          val name: String = attribute.attribute.name
          if !Html.reservedAttributes.contains(name) then attribute
          else Attribute(addPrefix(name)).optional.withValue(attribute.value)
        }
      )

      for
        // Note: if I  do this in one step, NoSuchElement gets into the error type...
        newElementAndTooltip: (ScalaXml.Element, ScalaXml.Nodes) <- elementToHtml(element)
        newElement: ScalaXml.Element = newElementAndTooltip._1
        tooltip: ScalaXml.Nodes = newElementAndTooltip._2
      yield
        val result: ScalaXml.Element = if newElement eq element then ScalaXml.setAttributes(
          element = if Html.reservedElements.contains(name) then ScalaXml.rename(element, addPrefix(name)) else element,
          attributes = attributes
        ) else ScalaXml.addAttributes(
          element = ScalaXml.declareNamespace(Html.namespace.default, newElement),
          attributes = Html.classAttribute.required.withValue(name) +: attributes
        )
        if tooltip.isEmpty then result else ScalaXml.setChildren(result,
          <span xmlns={Html.namespace.uri} class="tooltip">{tooltip}</span> +: ScalaXml.getChildren(result))

  yield result

  private def addPrefix(name: String): String = s"${namespace.getPrefix.get}-$name"

  private def isInNamespace(element: ScalaXml.Element): Boolean = ScalaXml.getNamespace(element) == namespace.default

  private def transformChildren[R](
    transform: ScalaXml.Element => URIO[R, ScalaXml.Element],
    element: ScalaXml.Element
  ): URIO[R, ScalaXml.Element] = for
    children: ScalaXml.Nodes <- URIO.foreach(ScalaXml.getChildren(element))((node: ScalaXml.Node) =>
      if !ScalaXml.isElement(node) then URIO.succeed(node) else transform(ScalaXml.asElement(node))
    )
  yield ScalaXml.setChildren(element, children)

object ToHtml:
  private val xml2htmlAttribute: Map[Attribute[String], Attribute[String]] = Map(
    Xml.idAttribute   -> Html.idAttribute,
    Xml.langAttribute -> Html.langAttribute
    // TODO xml:base? xml:space?
  )
