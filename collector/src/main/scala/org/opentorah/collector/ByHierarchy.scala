package org.opentorah.collector

import org.opentorah.store.{By, Context, Path, Pure}
import org.opentorah.xml.{Caching, Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class ByHierarchy(
  override val fromUrl: Element.FromUrl,
  selectorName: String,
  val hierarchyStores: Seq[Hierarchical]
) extends
  By.WithSelector[Hierarchical](selectorName),
  Pure[Hierarchical],
  Element.FromUrl.With:

  override def toString: String = s"ByHierarchy $selectorName [${fromUrl.url}]"

  override def storesPure: Seq[Hierarchical] = hierarchyStores

  // TODO generate hierarchy root index and reference it from the summary.
  // TODO allow viewing tree indexes rooted in intermediate ByHierarchys.

  def oneLevelIndex(context: Context, path: Path): Caching.Parser[ScalaXml.Element] =
    for
      content <- ZIO.foreach(hierarchyStores)((hierarchical: Hierarchical) =>
        val hierarchicalPath: Path = path :+ hierarchical
        for reference <- hierarchical.reference(context, hierarchicalPath)
        yield <li>{reference}</li>
      )
    yield
      <p>
        <l>{Hierarchical.displayName(this)}:</l>
        <ul>{content}</ul>
      </p>

  def treeIndex(path: Path, context: Context): Caching.Parser[ScalaXml.Element] =
    for
      content <- ZIO.foreach(hierarchyStores)((hierarchical: Hierarchical) =>
        val hierarchicalPath: Path = path :+ hierarchical
        for
          reference: ScalaXml.Element <- hierarchical.reference(context, hierarchicalPath)
          tree: Seq[ScalaXml.Element] <- ZIO.foreach(hierarchical.getBy.toSeq)((by: ByHierarchy) =>
            by.treeIndex(hierarchicalPath :+ by, context)
          )
        yield
          <li>
            {reference}
            {tree}
          </li>
      )
    yield
      <div class="tree-index">
        <ul>
          <li><em>{Hierarchical.displayName(this)}</em></li>
          <li><ul>{content}</ul></li>
        </ul>
      </div>

object ByHierarchy extends Element[ByHierarchy]("by"):
  override def contentParsable: Parsable[ByHierarchy] = new Parsable[ByHierarchy]:
    override def parser: Parser[ByHierarchy] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      selectorName: String <- By.selectorParser
      hierarchyStores: Seq[Hierarchical] <- Hierarchical.seq()
    yield ByHierarchy(
      fromUrl,
      selectorName,
      hierarchyStores
    )

    override def unparser: Unparser[ByHierarchy] = Unparser.concat(
      By.selectorUnparser,
      Hierarchical.seq(_.hierarchyStores)
    )
