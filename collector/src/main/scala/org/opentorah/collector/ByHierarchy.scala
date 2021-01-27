package org.opentorah.collector

import org.opentorah.xml.{Element, FromUrl, Parsable, Parser, Unparser, Xml}

final class ByHierarchy(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  val stores: Seq[Hierarchical]
) extends By with FromUrl.With {

  override def acceptsIndexHtml: Boolean = true

  override def findByName(name: String): Option[Store] = Store.findByName(name, stores)

  // TODO generate hierarchy root index and reference it from the summary.
  def oneLevelIndex(site: Site): Xml.Element =
    <p>
      <l>{displayName}:</l>
      <ul>{for (store <- stores) yield <li>{store.a(site)(text = store.displayTitle)}</li>}</ul>
    </p>

  def treeIndex(site: Site): Xml.Element = {
    <div class="tree-index">
      <ul>
        <li><em>{displayName}</em></li>
        <li>
          <ul>{
          for (store <- stores) yield
            <li>
              {store.a(site)(text = store.displayTitle)}
              {store.getBy.toSeq.map(_.treeIndex(site))}
            </li>
          }</ul>
        </li>
      </ul>
    </div>
  }
}

object ByHierarchy extends Element[ByHierarchy]("by") {

  override def contentParsable: Parsable[ByHierarchy] = new Parsable[ByHierarchy] {
    override def parser: Parser[ByHierarchy] = for {
      fromUrl <- Element.currentFromUrl
      selector <- By.selectorParser
      stores <- Hierarchical.followRedirects.seq()
    } yield new ByHierarchy(
      fromUrl,
      selector,
      stores
    )

    override def unparser: Unparser[ByHierarchy] = Unparser.concat(
      By.selectorUnparser,
      Hierarchical.seq(_.stores)
    )
  }
}
