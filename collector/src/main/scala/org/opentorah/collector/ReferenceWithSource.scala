package org.opentorah.collector

import org.opentorah.tei.EntityReference
import org.opentorah.util.Files
import org.opentorah.xml.{Antiparser, Attribute, Element, Parsable, Parser}

import scala.xml.Elem

// TODO generalize to hold unclears...
sealed class ReferenceWithSource(val path: Seq[String], val reference: EntityReference)

object ReferenceWithSource {

  private val pathAttribute: Attribute[String] = Attribute("path")

  final case class FromEntity(
    override val path: Seq[String],
    override val reference: EntityReference,
    entityId: String,
    entityName: String
  ) extends ReferenceWithSource(path, reference)

  private object FromEntityElement extends Element.WithToXml[FromEntity]("fromEntity") {
    private val entityIdAttribute: Attribute[String] = Attribute("entityId")
    private val entityNameAttribute: Attribute[String] = Attribute("entityName")

    override protected def parser: Parser[FromEntity] = for {
      path <- pathAttribute.required
      entityId <- entityIdAttribute.required
      entityName <- entityNameAttribute.required
      reference <- EntityReference.parsable.required
    } yield FromEntity(
      splitPath(path),
      reference,
      entityId,
      entityName
    )

    override protected def antiparser: Antiparser[FromEntity] = Antiparser.concat(
      pathAttribute.toXml.compose(unsplitPath),
      EntityReference.toXml.compose(_.reference),
      entityIdAttribute.toXml.compose(_.entityId),
      entityNameAttribute.toXml.compose(_.entityName)
    )
  }

  final case class FromDocument(
    override val path: Seq[String],
    override val reference: EntityReference,
    collectionFileName: String,
    collectionName: String,
    documentName: String
  ) extends ReferenceWithSource(path, reference) {
    def shortUrl: String = Files.mkUrl(shortPath)
    def shortPath: Seq[String] = Files.addExtension(
      Seq(CollectionObject.directoryName, collectionFileName, CollectionObject.documentsDirectoryName, documentName),
      "html"
    )
  }

  private object FromDocumentElement extends Element.WithToXml[FromDocument]("fromDocument") {
    private val collectionFileNameAttribute: Attribute[String] = Attribute("collectionFileName")
    private val collectionNameAttribute: Attribute[String] = Attribute("collectionName")
    private val documentNameAttribute: Attribute[String] = Attribute("documentName")

    override protected def parser: Parser[FromDocument] = for {
      path <- pathAttribute.required
      collectionFileName <- collectionFileNameAttribute.required
      collectionName <- collectionNameAttribute.required
      documentName <- documentNameAttribute.required
      reference <- EntityReference.parsable.required
    } yield FromDocument(
      splitPath(path),
      reference,
      collectionFileName,
      collectionName,
      documentName
    )

    override protected def antiparser: Antiparser[FromDocument] = Antiparser.concat(
      pathAttribute.toXml.compose(unsplitPath),
      EntityReference.toXml.compose(_.reference),
      collectionFileNameAttribute.toXml.compose(_.collectionFileName),
      collectionNameAttribute.toXml.compose(_.collectionName),
      documentNameAttribute.toXml.compose(_.documentName)
    )
  }

  final case class FromElement(
    override val path: Seq[String],
    override val reference: EntityReference,
  ) extends ReferenceWithSource(path, reference)

  private object FromElementElement extends Element.WithToXml[FromElement]("fromElement") {
    override protected def parser: Parser[FromElement] = for {
      path <- pathAttribute.required
      reference <- EntityReference.parsable.required
    } yield FromElement(
      splitPath(path),
      reference
    )

    // TODO why do I have to specify the type for compose() here?
    override protected def antiparser: Antiparser[FromElement] = Antiparser.concat(
      pathAttribute.toXml.compose[FromElement](unsplitPath),
      EntityReference.toXml.compose[FromElement](_.reference)
    )
  }

  private def splitPath(path: String): Seq[String] = path.split("/")

  private def unsplitPath(referenceWithSource: ReferenceWithSource): String = referenceWithSource.path.mkString("/")

  // TODO try making Parser, ToXml etc covariant? Generalize Union to handle subtypes?
  final def toXmlElement(value: ReferenceWithSource): Elem = value match {
    case value: FromEntity   => FromEntityElement.toXmlElement(value)
    case value: FromDocument => FromDocumentElement.toXmlElement(value)
    case value: FromElement  => FromElementElement.toXmlElement(value)
  }

  // TODO make Parsable covariant and reuse UnionParsable?
  object parsable extends Parsable[ReferenceWithSource] {
    override val name2parser: Map[String, Parsable.ContentTypeAndParser[ReferenceWithSource]] =
      FromEntityElement.name2parser ++ FromDocumentElement.name2parser ++ FromElementElement.name2parser
  }
}
