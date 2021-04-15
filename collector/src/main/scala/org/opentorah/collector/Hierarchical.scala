package org.opentorah.collector

import org.opentorah.tei.{Abstract, Body, Tei, Title}
import org.opentorah.site.{Caching, HtmlContent, Store}
import org.opentorah.xml.{Element, Elements, Xml}

trait Hierarchical extends Store with HtmlContent[Site] {
  def title: Title.Value

  final def titleString: String = Xml.toString(title.xml)

  def storeAbstract: Option[Abstract.Value]

  final def storeAbstractXmlElement: Seq[Xml.Element] =
    storeAbstract.toSeq.map(storeAbstract => Abstract.element.xmlElement(storeAbstract))

  def body: Option[Body.Value]

  def getBy: Option[ByHierarchy]

  final override def htmlHeadTitle: Option[String] = Some(titleString)

  final def displayTitle: String = displayName + ": " + titleString

  final def pathHeaderHorizontal(site: Site): String = {
    @scala.annotation.tailrec
    def pathHeaderHorizontal(path: Store.Path, result: Seq[String]): Seq[String] =
      if (path.isEmpty) result else pathHeaderHorizontal(
        path = path.tail.tail,
        result = result :+ s"${path.head.displayName} ${path.tail.head.displayName}"
      )

    pathHeaderHorizontal(site.store2path(this), Seq.empty).mkString(", ")
  }

  final override def content(site: Site): Caching.Parser[Xml.Element] = {
    @scala.annotation.tailrec
    def pathHeaderVertical(path: Store.Path, result: Seq[Xml.Element]): Seq[Xml.Element] =
      if (path.isEmpty) result else pathHeaderVertical(
        path = path.tail.tail,
        result = result :+ {
          val hierarchy: Hierarchy = path.tail.head.asInstanceOf[Hierarchy]
          <l>{path.head.displayName} {hierarchy.a(site)(text = hierarchy.displayName)}: {hierarchy.title.xml}</l>
        }
      )

    val path: Store.Path = site.store2path(this)

    innerContent(site).map(inner =>
      <div>
        <div class="store-header">
          {pathHeaderVertical(path.init.init, Seq.empty)}
          <head xmlns={Tei.namespace.uri}>{path.init.last.displayName} {displayName}: {title.xml}</head>
          {storeAbstractXmlElement}
          {body.toSeq.map(_.xml)}
        </div>
        {inner}
      </div>
    )
  }

  protected def innerContent(site: Site): Caching.Parser[Xml.Element]

  def flatIndexEntry(site: Site): Xml.Element =
    <div>
      {a(site)(text = pathHeaderHorizontal(site) + ": " + titleString)}
      {storeAbstractXmlElement}
    </div>
}

object Hierarchical extends Elements.Union[Hierarchical] {

  protected def elements: Seq[Element[_ <: Hierarchical]] = Seq(Hierarchy, Collection)

  override protected def elementByValue(value: Hierarchical): Element[_] = value match {
    case _: Hierarchy  => Hierarchy
    case _: Collection => Collection
  }
}
