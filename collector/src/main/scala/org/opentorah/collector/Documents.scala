package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.store.Directory
import org.opentorah.tei.Tei
import org.opentorah.util.Collections
import org.opentorah.xml.{Element, Parser, ScalaXml}

import java.net.URL

final class Documents(
  collection: Collection,
  override val fromUrl: Element.FromUrl,
  directory: String,
  parts: Seq[CollectionPart]
) extends Directory[Tei, Document, Documents.All](
  directory,
  "xml",
  Document,
  Documents.All(_, parts)
):
  // having this override for Directory.names is a fair price to pay for not having Store/Stores duality...
  override def names: Names = collection.names

  override protected def loadFile(url: URL): Parser[Tei] = Tei.parse(url, ScalaXml)

object Documents:

  final class All(
    name2entry: Map[String, Document],
    partsRaw: Seq[CollectionPart]
  ) extends Directory.Wrapper[Document](name2entry):

    lazy val originalDocuments: Seq[Document] = stores
      .filterNot(_.isTranslation)
      .sortBy(_.baseName)

    lazy val translations: Map[String, Seq[Document]] = Collections.mapValues(stores
      .filter(_.isTranslation)
      .map(document => (document.baseName, document))
      .groupBy(_._1))(_.map(_._2))

    lazy val siblings: Map[String, (Option[Document], Option[Document])] =
      Collections.prevAndNext(originalDocuments)
        .map((document, siblings) => (document.baseName, siblings))
        .toMap

    lazy val parts: Seq[CollectionPart.Part] = CollectionPart.getParts(partsRaw, originalDocuments)
