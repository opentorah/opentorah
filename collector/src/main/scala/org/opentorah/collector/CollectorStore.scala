package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.store.{Selector, StoreNg, Urls}
import org.opentorah.tei.Title
import org.opentorah.xml.{Attribute, Parser}

import scala.xml.Node

final class CollectorStore(
  urls: Urls,
  inheritedSelectors: Seq[Selector],
  names: Names,
  title: Title.Value,
  override val definedSelectors: Seq[Selector]
//  entities: Option[Entities.Element]
) extends StoreNg.Base(
  urls,
  inheritedSelectors,
  names
) {
  override def companion: CollectorStore.type = CollectorStore
}

object CollectorStore extends StoreNg {

  override type Instance = CollectorStore

  override protected def parser(
    urls: Urls,
    inheritedSelectors: Seq[Selector]
  ): Parser[Instance] = for {
    names <- Names.withDefaultNameParser
    title <- Title.parsable.required
    selectors <- Selector.all
//    entities <- Entities.parsable.optional
//    by <- By.parsable.optional
  } yield new CollectorStore(
    urls,
    inheritedSelectors,
    names,
    title,
    selectors
//    entities,
//    by
  )

  override protected def attributes(value: Instance): Seq[Attribute.Value[_]] =
    Names.antiparser.attributes(value.names)

  // TODO take Inline descendants into account!
  override protected def content(value: Instance): Seq[Node] =
    Names.antiparser.content(value.names)
}
