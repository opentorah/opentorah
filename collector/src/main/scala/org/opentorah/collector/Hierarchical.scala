package org.opentorah.collector

import org.opentorah.metadata.Language
import org.opentorah.tei.{Abstract, Body, Tei, Title}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Caching, Store, Stores}
import org.opentorah.xml.{Element, Elements, ScalaXml}

// TODO abstract store2path into Site;
// push Hierarchical/ByHierarchy into Site;
// push up Collector-specific stuff (if any);
// do 'texts' site!
trait Hierarchical extends Store.NonTerminal, Stores.Pure, HtmlContent[Collector]:
  def title: Title.Value

  final def titleString: String = ScalaXml.toString(title.content)

  def storeAbstract: Option[Abstract.Value]

  final def storeAbstractXmlElement: Seq[ScalaXml.Element] =
    storeAbstract.toSeq.map(storeAbstract => Abstract.element.xmlElement(storeAbstract))

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

  final override def content(collector: Collector): Caching.Parser[ScalaXml.Element] =
    @scala.annotation.tailrec
    def pathHeaderVertical(path: Store.Path, result: Seq[ScalaXml.Element]): Seq[ScalaXml.Element] =
      if path.isEmpty then result else pathHeaderVertical(
        path = path.tail.tail,
        result = result :+ {
          val hierarchy: Hierarchy = path.tail.head.asInstanceOf[Hierarchy]
          <l>{Hierarchical.displayName(path.head)} {hierarchy.a(collector)(text = Hierarchical.displayName(hierarchy))}: {hierarchy.title.content}</l>
        }
      )

    val path: Store.Path = collector.store2path(this)

    innerContent(collector).map(inner =>
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

  protected def innerContent(collector: Collector): Caching.Parser[ScalaXml.Element]

  def flatIndexEntry(collector: Collector): ScalaXml.Element =
    <div>
      {a(collector)(text = pathHeaderHorizontal(collector) + ": " + titleString)}
      {storeAbstractXmlElement}
    </div>

object Hierarchical extends Elements.Union[Hierarchical]:

  protected def elements: Seq[Element[? <: Hierarchical]] = Seq(Hierarchy, Collection)

  override protected def elementByValue(value: Hierarchical): Element[?] = value match
    case _: Hierarchy  => Hierarchy
    case _: Collection => Collection

  def displayName(store: Store): String = store.names.doFind(Language.Russian.toSpec).name
