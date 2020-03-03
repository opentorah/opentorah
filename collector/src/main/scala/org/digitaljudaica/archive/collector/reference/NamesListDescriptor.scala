package org.digitaljudaica.archive.collector.reference

import java.io.File
import org.digitaljudaica.reference.Entity
import org.digitaljudaica.xml.{ContentType, From, Parser, Xml}

// TODO move into store, rename and extend Descriptor
final case class NamesListDescriptor(
  entity: Entity,
  id: String,
  role: Option[String],
  head: String
) {
  def fillOut(allNameds: Seq[Named]): NamesList = new NamesList(
    entity,
    id,
    role,
    head,
    nameds = allNameds.filter(named => named.entity == entity && named.role == role)
  )
}

object NamesListDescriptor {

  private val parser: Parser[NamesListDescriptor] = for {
    name <- Xml.name
    entity: Entity = Entity.forList(name).get
    id <- Xml.attribute.required.id
    role <- Xml.attribute.optional("role")
    head <- Xml.required("head", ContentType.Text, Xml.text.required)
  } yield NamesListDescriptor(
    entity,
    id,
    role,
    head
  )

  def readAll(directory: File, fileName: String): (String, Seq[NamesListDescriptor]) = Parser.parseDo(
    From.file(directory, fileName)
      .parse(Xml.withName("names", for {
        head <- Xml.required("head", ContentType.Text, Xml.text.required) // TODO common combinator
        listDescriptors <- Xml.all(parser)
      } yield (head, listDescriptors)))
  )
}
