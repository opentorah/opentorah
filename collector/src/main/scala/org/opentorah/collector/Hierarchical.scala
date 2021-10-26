package org.opentorah.collector

import org.opentorah.metadata.{Language, Names}
import org.opentorah.tei.{Abstract, Body, Tei, Title}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Path, Pure, Store}
import org.opentorah.xml.{Caching, Element, Elements, Parsable, ScalaXml}

// TODO push Hierarchical/ByHierarchy into Site;
// TODO push up Collector-specific stuff (if any);
// TODO do 'texts' site!
abstract class Hierarchical(
  override val fromUrl: Element.FromUrl,
  override val names: Names,
  val title: Title.Value,
  val description: Option[Abstract.Value],
  val body: Option[Body.Value],
) extends
  Element.FromUrl.With,
  Pure[Store],
  HtmlContent.HierarchyViewer[Collector]:

  final def titleString: String = title.content.toString

  final override def htmlHeadTitle: Option[String] = Some(titleString)
  final override def htmlBodyTitle: Option[ScalaXml.Nodes] = None

  final def displayTitle: String = Hierarchical.displayName(this) + ": " + titleString

  final def descriptionNodes: ScalaXml.Nodes = description.toSeq.map(Abstract.element.xmlElement)

  final def pathHeaderHorizontal(path: Path): String =
    @scala.annotation.tailrec
    def pathHeaderHorizontal(path: Path, result: Seq[String]): Seq[String] =
      if path.isEmpty then result else pathHeaderHorizontal(
        path = path.tail.tail,
        result = result :+ s"${Hierarchical.displayName(path.head)} ${Hierarchical.displayName(path.tail.head)}"
      )

    pathHeaderHorizontal(path, Seq.empty).mkString(", ")

  final def pathHeaderVertical(path: Path, collector: Collector): Seq[ScalaXml.Element] =
    @scala.annotation.tailrec
    def pathHeaderVertical(
      path: Path,
      pathTail: Path,
      result: Seq[ScalaXml.Element]
    ): Seq[ScalaXml.Element] = if pathTail.isEmpty then result else
      val pathNew: Path = path ++ pathTail.take(2)
      val hierarchical: Hierarchical = pathTail.tail.head.asInstanceOf[Hierarchical]
      // TODO drop the colon if the title is empty
      val line: ScalaXml.Element =
        <l>
          {Hierarchical.displayName(pathTail.head)}
          {collector.a(pathNew)(text = Hierarchical.displayName(hierarchical))}:
          {hierarchical.title.content.scalaXml}
        </l>
      pathHeaderVertical(
        path = pathNew,
        pathTail = pathTail.drop(2),
        result = result :+ line
      )

    pathHeaderVertical(path = Seq.empty, pathTail = path, result = Seq.empty)

  final override def content(path: Path, collector: Collector): Caching.Parser[ScalaXml.Element] =
    innerContent(path, collector).map(inner =>
      <div>
        <div class="store-header">
          {pathHeaderVertical(path.dropRight(2), collector)}
          <head xmlns={Tei.namespace.uri}>
            {Hierarchical.displayName(path.init.last)}
            {Hierarchical.displayName(this)}:
            {title.content.scalaXml}
          </head>
          {descriptionNodes}
          {body.toSeq.map(_.content.scalaXml)}
        </div>
        {inner}
      </div>
    )

  def getBy: Option[ByHierarchy]

  protected def innerContent(path: Path, collector: Collector): Caching.Parser[ScalaXml.Element]

object Hierarchical extends Elements.Union[Hierarchical]:
  protected def elements: Seq[Element[? <: Hierarchical]] = Seq(Hierarchy, Collection)

  override protected def elementByValue(value: Hierarchical): Element[?] = value match
    case _: Hierarchy  => Hierarchy
    case _: Collection => Collection

  val namesParsable: Parsable[Names] = Names.withDefaultNameParsable
  val titleElement: Elements.Required[Title.Value] = Title.element.required
  val descriptionElement: Elements.Optional[Abstract.Value] = Abstract.element.optional
  val bodyElement: Elements.Optional[Body.Value] = Body.element.optional

  // TODO move
  def displayName(store: Store): String = store.names.doFind(Language.Russian.toSpec).name
