package org.opentorah.collector

import org.opentorah.store.{By, Selector, Store, Stores}
import org.opentorah.xml.{Element, FromUrl, Parsable, Parser, ScalaXml, Unparser}

final class ByHierarchy(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  val stores: Seq[Hierarchical]
) extends By with Stores.NonTerminal with FromUrl.With {

  override protected def nonTerminalStores: Seq[Store.NonTerminal] = stores

  // TODO generate hierarchy root index and reference it from the summary.
  def oneLevelIndex(collector: Collector): ScalaXml.Element =
    <p>
      <l>{Hierarchical.displayName(this)}:</l>
      <ul>{stores.map(store => <li>{store.a(collector)(text = store.displayTitle)}</li>)}</ul>
    </p>

  def treeIndex(collector: Collector): ScalaXml.Element = {
    <div class="tree-index">
      <ul>
        <li><em>{Hierarchical.displayName(this)}</em></li>
        <li>
          <ul>{stores.map(store =>
            <li>
              {store.a(collector)(text = store.displayTitle)}
              {store.getBy.toSeq.map(_.treeIndex(collector))}
            </li>
          )}</ul>
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
