package org.opentorah.collector

import org.opentorah.metadata.Language
import org.opentorah.tei.{Abstract, Body, Tei, Title}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Caching, Store}
import org.opentorah.xml.{Element, Elements, ScalaXml}

// TODO abstract store2path into Site;
// push Hierarchical/ByHierarchy into Site;
// push up Collector-specific stuff (if any);
// do 'texts' site!
trait Hierarchical extends Store.Pure[Store], HtmlContent.HierarchyViewer[Collector]:
  def title: Title.Value

  final def titleString: String = title.content.toString

  def storeAbstract: Option[Abstract.Value]

  final def storeAbstractXmlElement: Seq[ScalaXml.Element] = storeAbstract.toSeq.map(Abstract.element.xmlElement)

  def body: Option[Body.Value]

  def getBy: Option[ByHierarchy]

  final override def htmlHeadTitle: Option[String] = Some(titleString)

  final def displayTitle: String = Hierarchical.displayName(this) + ": " + titleString

  final def pathHeaderHorizontal(collector: Collector): String =
    @scala.annotation.tailrec
    def pathHeaderHorizontal(path: Store.Path, result: Seq[String]): Seq[String] =
      if path.isEmpty then result else pathHeaderHorizontal(
        path = path.tail.tail,
        result = result :+ s"${Hierarchical.displayName(path.head)} ${Hierarchical.displayName(path.tail.head)}"
      )

    pathHeaderHorizontal(collector.store2path(this), Seq.empty).mkString(", ")

  final override def content(path: Store.Path, collector: Collector): Caching.Parser[ScalaXml.Element] =
    @scala.annotation.tailrec
    def pathHeaderVertical(path: Store.Path, result: Seq[ScalaXml.Element]): Seq[ScalaXml.Element] =
      if path.isEmpty then result else pathHeaderVertical(
        path = path.tail.tail,
        result = result :+ {
          val hierarchy: Hierarchy = path.tail.head.asInstanceOf[Hierarchy]
          // TODO carefully remove the use of collector.path() below:
          <l>{Hierarchical.displayName(path.head)} {HtmlContent.a(collector.hierarchicalPath(hierarchy))(text = Hierarchical.displayName(hierarchy))}: {hierarchy.title.content}</l>
        }
      )

    // TODO carefully remove the use of collector.store2path() below:
    val path: Store.Path = collector.store2path(this)

    innerContent(path, collector).map(inner =>
      <div>
        <div class="store-header">
          {pathHeaderVertical(path.init.init, Seq.empty)}
          <head xmlns={Tei.namespace.uri}>{Hierarchical.displayName(path.init.last)} {Hierarchical.displayName(this)}: {title.content}</head>
          {storeAbstractXmlElement}
          {body.toSeq.map(_.content)}
        </div>
        {inner}
      </div>
    )

  protected def innerContent(path: Store.Path, collector: Collector): Caching.Parser[ScalaXml.Element]

  def flatIndexEntry(collector: Collector): ScalaXml.Element =
    <div>
      {HtmlContent.a(collector.hierarchicalPath(this))(text = pathHeaderHorizontal(collector) + ": " + titleString)}
      {storeAbstractXmlElement}
    </div>

object Hierarchical extends Elements.Union[Hierarchical]:
  protected def elements: Seq[Element[? <: Hierarchical]] = Seq(Hierarchy, Collection)

  override protected def elementByValue(value: Hierarchical): Element[?] = value match
    case _: Hierarchy  => Hierarchy
    case _: Collection => Collection

  def displayName(store: Store): String = store.names.doFind(Language.Russian.toSpec).name
