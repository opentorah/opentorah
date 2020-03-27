package org.opentorah.store

import java.net.URL
import org.opentorah.entity.EntityReference
import org.opentorah.metadata.Names
import org.opentorah.util.Files
import org.opentorah.xml.Parser
import scala.xml.Node

abstract class Store(
  inheritedSelectors: Seq[Selector],
  val fromUrl: Option[URL],
  val baseUrl: URL
) extends WithSelectors(inheritedSelectors) {

  def names: Names

  override def toString: String = names.name

  def entities: Option[Entities] = None

  def by: Option[By[_]] = None

  def title: Option[StoreElement.Title.Value] = None

  def storeAbstract: Option[StoreElement.Abstract.Value] = None

  def notes: StoreElement.Notes = new StoreElement.Notes(Seq.empty)

  def references: Seq[EntityReference]

  final def withPath[R](
    path: Path = Path.empty,
    values: Store => Seq[R]
  ): Seq[WithPath[R]] = {
    val fromStore: Seq[WithPath[R]] =
      values(this).map(WithPath[R](path, _))

    val fromEntities: Seq[WithPath[R]] = entities.toSeq.flatMap(entities =>
      entities.withPath[R](path :+ entities.selector.bind(entities), values))

    val fromBy: Seq[WithPath[R]] =
      by.toSeq.flatMap(_.withPath[R](path, values))

    fromEntities ++ fromStore ++ fromBy
  }
}

object Store {

  class FromElement(
    inheritedSelectors: Seq[Selector],
    fromUrl: Option[URL],
    baseUrl: URL,
    element: StoreElement.Inline
  ) extends Store(inheritedSelectors, fromUrl, baseUrl) {

    final override def names: Names =
      element.names

    final override protected def definedSelectors: Seq[Selector] =
      element.selectors

    final override val entities: Option[Entities] =
      element.entities.map(entities => new Entities(selectors, baseUrl, entities))

    final override def title: Option[StoreElement.Title.Value] =
      element.title

    final override def storeAbstract: Option[StoreElement.Abstract.Value] =
      element.storeAbstract

    final override def notes: StoreElement.Notes =
      element.notes

    override def by: Option[By[_]] =
      element.by.map(byElement => new By(selectors, baseUrl, byElement))

    final override def references: Seq[EntityReference] = {
      val lookInto: Seq[Node] =
        title.map(_.xml).getOrElse(Seq.empty) ++
        storeAbstract.map(_.xml).getOrElse(Seq.empty) ++
        notes.xml

      lookInto.flatMap(EntityReference.parsable.descendants)
    }
  }

  def read[S <: Store](
    fromUrl: URL,
    inheritedSelectors: Seq[Selector] = Selector.predefinedSelectors
  ): S = fromElement[S](
    inheritedSelectors,
    fromUrl = Some(fromUrl),
    baseUrl = fromUrl,
    element = Parser.parseDo(StoreElement.parse(fromUrl))
  )

  private [store] def fromElement[S <: Store](
    inheritedSelectors: Seq[Selector],
    fromUrl: Option[URL],
    baseUrl: URL,
    element: StoreElement
  ): S = element match {
    case StoreElement.FromFile(file) => read[S](
      fromUrl = Files.fileInDirectory(baseUrl, file),
      inheritedSelectors
    )

    case element: StoreElement.Inline =>
      if (element.storeType.isDefined) Class.forName(element.storeType.get)
        .getConstructor(
          classOf[Seq[Selector]],
          classOf[Option[URL]],
          classOf[URL],
          classOf[StoreElement.Inline]
        )
        .newInstance(
          inheritedSelectors,
          fromUrl,
          baseUrl,
          element
        )
        .asInstanceOf[S]
      else new FromElement(
        inheritedSelectors,
        fromUrl,
        baseUrl,
        element
      )
        .asInstanceOf[S]
  }
}
