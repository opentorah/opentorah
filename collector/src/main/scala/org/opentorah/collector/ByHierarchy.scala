package org.opentorah.collector

import org.opentorah.store.{By, Stores}
import org.opentorah.xml.{Element, Parsable, Parser, ScalaXml, Unparser}

final class ByHierarchy(
  override val fromUrl: Element.FromUrl,
  selectorName: String,
  val hierarchyStores: Seq[Hierarchical]
) extends By.WithSelector[Hierarchical](selectorName), Stores.Pure[Hierarchical], Element.FromUrl.With:

  override def storesPure: Seq[Hierarchical] = hierarchyStores

  // TODO generate hierarchy root index and reference it from the summary.
  def oneLevelIndex(collector: Collector): ScalaXml.Element =
    <p>
      <l>{Hierarchical.displayName(this)}:</l>
      <ul>{hierarchyStores.map(store => <li>{store.a(collector)(text = store.displayTitle)}</li>)}</ul>
    </p>

  def treeIndex(collector: Collector): ScalaXml.Element =
    <div class="tree-index">
      <ul>
        <li><em>{Hierarchical.displayName(this)}</em></li>
        <li>
          <ul>{hierarchyStores.map(store =>
            <li>
              {store.a(collector)(text = store.displayTitle)}
              {store.getBy.toSeq.map(_.treeIndex(collector))}
            </li>
          )}</ul>
        </li>
      </ul>
    </div>

object ByHierarchy extends Element[ByHierarchy]("by"):

  override def contentParsable: Parsable[ByHierarchy] = new Parsable[ByHierarchy]:
    override def parser: Parser[ByHierarchy] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      selectorName: String <- By.selectorParser
      hierarchyStores: Seq[Hierarchical] <- Hierarchical.followRedirects.seq()
    yield ByHierarchy(
      fromUrl,
      selectorName,
      hierarchyStores
    )

    override def unparser: Unparser[ByHierarchy] = Unparser.concat(
      By.selectorUnparser,
      Hierarchical.seq(_.hierarchyStores)
    )
