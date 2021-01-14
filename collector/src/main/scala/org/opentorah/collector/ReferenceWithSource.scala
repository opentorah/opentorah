package org.opentorah.collector

import org.opentorah.tei.EntityReference
import org.opentorah.util.Files
import org.opentorah.xml.{Unparser, Attribute, Element, Elements, Parsable, Parser}

// TODO generalize to hold unclears...

sealed abstract class ReferenceWithSource(val path: Seq[String], val reference: EntityReference)

object ReferenceWithSource extends Elements.Union[ReferenceWithSource] {

  private val pathAttribute: Attribute.Required[String] = Attribute("path").required

  final case class FromEntity(
    override val path: Seq[String],
    override val reference: EntityReference,
    entityId: String,
    entityName: String
  ) extends ReferenceWithSource(path, reference)

  object FromEntity extends Element[FromEntity]("fromEntity") {
    private val entityIdAttribute: Attribute.Required[String] = Attribute("entityId").required
    private val entityNameAttribute: Attribute.Required[String] = Attribute("entityName").required

    override def contentParsable: Parsable[FromEntity] = new Parsable[FromEntity] {
      override def parser: Parser[FromEntity] = for {
        path <- pathAttribute()
        entityId <- entityIdAttribute()
        entityName <- entityNameAttribute()
        reference <- EntityReference.required()
      } yield FromEntity(
        splitPath(path),
        reference,
        entityId,
        entityName
      )

      override def unparser: Unparser[FromEntity] = Unparser.concat(
        pathAttribute(unsplitPath),
        EntityReference.required(_.reference),
        entityIdAttribute(_.entityId),
        entityNameAttribute(_.entityName)
      )
    }
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
    private val collectionFileNameAttribute: Attribute.Required[String] = Attribute("collectionFileName").required
    private val collectionNameAttribute: Attribute.Required[String] = Attribute("collectionName").required
    private val documentNameAttribute: Attribute.Required[String] = Attribute("documentName").required

    override def contentParsable: Parsable[FromDocument] = new Parsable[FromDocument] {
      override def parser: Parser[FromDocument] = for {
        path <- pathAttribute()
        collectionFileName <- collectionFileNameAttribute()
        collectionName <- collectionNameAttribute()
        documentName <- documentNameAttribute()
        reference <- EntityReference.required()
      } yield FromDocument(
        splitPath(path),
        reference,
        collectionFileName,
        collectionName,
        documentName
      )

      override def unparser: Unparser[FromDocument] = Unparser.concat(
        pathAttribute(unsplitPath),
        EntityReference.required(_.reference),
        collectionFileNameAttribute(_.collectionFileName),
        collectionNameAttribute(_.collectionName),
        documentNameAttribute(_.documentName)
      )
    }
  }

  final case class FromElement(
    override val path: Seq[String],
    override val reference: EntityReference,
  ) extends ReferenceWithSource(path, reference)

  object FromElement extends Element[FromElement]("fromElement") {
    override def contentParsable: Parsable[FromElement] = new Parsable[FromElement] {
      override def parser: Parser[FromElement] = for {
        path <- pathAttribute()
        reference <- EntityReference.required()
      } yield FromElement(
        splitPath(path),
        reference
      )

      override def unparser: Unparser[FromElement] = Unparser.concat(
        pathAttribute(unsplitPath),
        EntityReference.required(_.reference)
      )
    }
  }

  private def splitPath(path: String): Seq[String] = path.split("/").toIndexedSeq

  private def unsplitPath(referenceWithSource: ReferenceWithSource): String = referenceWithSource.path.mkString("/")

  override protected val elements: Seq[Element[_ <: ReferenceWithSource]] = Seq(FromEntity, FromDocument, FromElement)

  override protected def elementByValue(value: ReferenceWithSource): Element[_ <: ReferenceWithSource] = value match {
    case _: FromEntity   => FromEntity
    case _: FromDocument => FromDocument
    case _: FromElement  => FromElement
  }
}
