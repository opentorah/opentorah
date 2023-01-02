package org.opentorah.html

import org.opentorah.xml.{Attribute, Namespace, ScalaXml, Xml}
import zio.{URIO, ZIO}

// Converting other XML dialects (e.g., TEI, DocBook) to HTML
trait ToHtml[R]:
  protected def namespace: Namespace

  // TODO handle table/tr/td systematically

  protected def isFootnote(element: ScalaXml.Element): Boolean

  protected def isFootnotesContainer(element: ScalaXml.Element): Boolean = false

  protected def elementToHtml(element: ScalaXml.Element): URIO[R, ScalaXml.Element]

  private def isInNamespace(element: ScalaXml.Element): Boolean = ScalaXml.getNamespace(element) == namespace.default

  private def addPrefix(name: String): String = s"${namespace.getPrefix.get}-$name"

  final def toHtml(element: ScalaXml.Element): URIO[R, ScalaXml.Element] =
    for
      footnotesDone <- processFootnotes(element).provideSomeLayer[R](Footnotes.empty)
      html <- elementsToHtml(footnotesDone)
    yield
      html

  private def processFootnotes(element: ScalaXml.Element): URIO[Footnotes & R, ScalaXml.Element] = for
    isEmpty: Boolean <- Footnotes.isEmpty
    doPush: Boolean = isEmpty || (isInNamespace(element) && isFootnotesContainer(element))
    _ <- if !doPush then ZIO.succeed(()) else Footnotes.push
    newElement: ScalaXml.Element <-
      if isInNamespace(element) && isFootnote(element)
      then Footnotes.footnote(element)
      else transformChildren(processFootnotes, element)
    result: ScalaXml.Element <-
      if !doPush
      then ZIO.succeed(newElement)
      else processLevels(newElement, Seq.empty)
  yield result

  private def processLevels(
    newElement: ScalaXml.Element,
    levels: Seq[Seq[ScalaXml.Element]]
  ): URIO[Footnotes & R, ScalaXml.Element] = for
    footnotes: Seq[ScalaXml.Element] <- Footnotes.get
    result: ScalaXml.Element <-
      if footnotes.isEmpty then for
        _ <- Footnotes.pop
      yield
        if levels.isEmpty
        then newElement
        else ScalaXml.appendChildren(newElement,
          (for (level, depth) <- levels.zipWithIndex yield Html.footnoteLevel(level, depth)).flatten
        )
      else for
        nextLevel: Seq[ScalaXml.Element] <- ZIO.foreach(footnotes)(processFootnotes)
        result <- processLevels(newElement, levels :+ nextLevel)
      yield result
  yield result

  private def elementsToHtml(oldElement: ScalaXml.Element): URIO[R, ScalaXml.Element] = for
    element: ScalaXml.Element <- transformChildren(elementsToHtml, oldElement)
    result: ScalaXml.Element <- if !isInNamespace(element) then ZIO.succeed(element) else

      val attributes: Attribute.StringValues =
        for attribute: Attribute.Value[String] <- ScalaXml.getAttributes(element) yield
          ToHtml.xml2htmlAttribute.get(attribute.attribute).map(_.optional.withValue(attribute.value)).getOrElse {
            val name: String = attribute.attribute.name
            if !Html.reservedAttributes.contains(name) then attribute
            else Attribute(addPrefix(name)).optional.withValue(attribute.value)
          }

      for newElement: ScalaXml.Element <- elementToHtml(element) yield
        if isInNamespace(newElement) then
          val name: String = ScalaXml.getName(newElement)
          ScalaXml.setAttributes(
            element = if !Html.reservedElements.contains(name) then newElement else ScalaXml.rename(newElement, addPrefix(name)),
            attributes = attributes
          )
        else
          ScalaXml.addAttributes(
            element = newElement,
            attributes = Html.classAttribute.required.withValue(ScalaXml.getName(element)) +: attributes
          )

  yield result

  private def transformChildren[T](
    transform: ScalaXml.Element => URIO[T, ScalaXml.Element],
    element: ScalaXml.Element
  ): URIO[T, ScalaXml.Element] = for
    children: ScalaXml.Nodes <- ZIO.foreach(ScalaXml.getChildren(element))((node: ScalaXml.Node) =>
      if !ScalaXml.isElement(node) then ZIO.succeed(node) else transform(ScalaXml.asElement(node))
    )
  yield ScalaXml.setChildren(element, children)

object ToHtml:
  private val xml2htmlAttribute: Map[Attribute[String], Attribute[String]] = Map(
    Xml.idAttribute   -> Html.idAttribute,
    Xml.langAttribute -> Html.langAttribute
    // TODO xml:base? xml:space?
  )

  // TODO eliminate
  def namespace(element: ScalaXml.Element): ScalaXml.Element =
    ScalaXml.declareNamespace(Html.namespace.default, element)
