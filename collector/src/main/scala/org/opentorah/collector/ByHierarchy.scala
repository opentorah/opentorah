package org.opentorah.collector

import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Store, Stores}
import org.opentorah.xml.{Element, Parsable, Parser, ScalaXml, Unparser}

final class ByHierarchy(
  override val fromUrl: Element.FromUrl,
  selectorName: String,
  val hierarchyStores: Seq[Hierarchical]
) extends By.WithSelector[Hierarchical](selectorName), Stores.Pure[Hierarchical], Element.FromUrl.With:

  override def storesPure: Seq[Hierarchical] = hierarchyStores

  // TODO generate hierarchy root index and reference it from the summary.
  // TODO use path and path :+ hierarchical to avoid calling Collector for the path
  def oneLevelIndex(path: Store.Path, collector: Collector): ScalaXml.Element =
    <p>
      <l>{Hierarchical.displayName(this)}:</l>
      <ul>{for
        hierarchical <- hierarchyStores
      yield
        <li>{HtmlContent.a(collector.hierarchicalPath(hierarchical))(text = hierarchical.displayTitle)}</li>
      }</ul>
    </p>

  def treeIndex(path: Store.Path, collector: Collector): ScalaXml.Element =
    <div class="tree-index">
      <ul>
        <li><em>{Hierarchical.displayName(this)}</em></li>
        <li>
          <ul>{for hierarchical <- hierarchyStores yield
            <li>
              {HtmlContent.a(collector.hierarchicalPath(hierarchical))(text = hierarchical.displayTitle)}
              {hierarchical.getBy.toSeq.map(_.treeIndex(path :+ hierarchical, collector))}
            </li>
          }</ul>
        </li>
      </ul>
    </div>

  // TODO factor out private def forHierarchical(hierachical) with its link and title.

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
