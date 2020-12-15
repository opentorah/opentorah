package org.opentorah.collector

import org.opentorah.tei.EntityReference
import org.opentorah.util.Files
import org.opentorah.xml.{Antiparser, Attribute, Element, Parser, ToXml, Xml}

// TODO generalize to hold unclears...

sealed abstract class ReferenceWithSource(val path: Seq[String], val reference: EntityReference)

object ReferenceWithSource extends Element.Union[ReferenceWithSource] with ToXml[ReferenceWithSource] {

  private val pathAttribute: Attribute[String] = Attribute("path")

  final case class FromEntity(
    override val path: Seq[String],
    override val reference: EntityReference,
    entityId: String,
    entityName: String
  ) extends ReferenceWithSource(path, reference)

  object FromEntity extends Element[FromEntity]("fromEntity") {
    private val entityIdAttribute: Attribute[String] = Attribute("entityId")
    private val entityNameAttribute: Attribute[String] = Attribute("entityName")

    override def parser: Parser[FromEntity] = for {
      path <- pathAttribute.required
      entityId <- entityIdAttribute.required
      entityName <- entityNameAttribute.required
      reference <- EntityReference.required
    } yield FromEntity(
      splitPath(path),
      reference,
      entityId,
      entityName
    )

    override def antiparser: Antiparser[FromEntity] = Antiparser.concat(
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

  object FromDocument extends Element[FromDocument]("fromDocument") {
    private val collectionFileNameAttribute: Attribute[String] = Attribute("collectionFileName")
    private val collectionNameAttribute: Attribute[String] = Attribute("collectionName")
    private val documentNameAttribute: Attribute[String] = Attribute("documentName")

    override def parser: Parser[FromDocument] = for {
      path <- pathAttribute.required
      collectionFileName <- collectionFileNameAttribute.required
      collectionName <- collectionNameAttribute.required
      documentName <- documentNameAttribute.required
      reference <- EntityReference.required
    } yield FromDocument(
      splitPath(path),
      reference,
      collectionFileName,
      collectionName,
      documentName
    )
    override def antiparser: Antiparser[FromDocument] = Antiparser.concat(
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

  object FromElement extends Element[FromElement]("fromElement") {
    override def parser: Parser[FromElement] = for {
      path <- pathAttribute.required
      reference <- EntityReference.required
    } yield FromElement(
      splitPath(path),
      reference
    )

    override def antiparser: Antiparser[FromElement] = Antiparser.concat(
      pathAttribute.toXml.compose(unsplitPath),
      EntityReference.toXml.compose(_.reference)
    )
  }

  private def splitPath(path: String): Seq[String] = path.split("/")

  private def unsplitPath(referenceWithSource: ReferenceWithSource): String = referenceWithSource.path.mkString("/")

  override protected val elements: Seq[Element[_ <: ReferenceWithSource]] = Seq(FromEntity, FromDocument, FromElement)

  // TODO extend Union from ToXml:
  override def toXmlElement(value: ReferenceWithSource): Xml.Element = value match {
    case value: FromEntity   => FromEntity  .toXmlElement(value)
    case value: FromDocument => FromDocument.toXmlElement(value)
    case value: FromElement  => FromElement .toXmlElement(value)
  }
}
