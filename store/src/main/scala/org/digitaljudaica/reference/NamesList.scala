package org.digitaljudaica.reference

import java.io.File
import org.digitaljudaica.xml.{From, Parser, Xml}

final case class NamesList(
  entity: Entity,
  id: String,
  role: Option[String],
  head: String
)

// TODO extend Descriptor; normalize parsers
object NamesList {

  val parser: Parser[NamesList] = for {
    name <- Xml.name
    entity: Entity = Entity.forList(name).get
    id <- Xml.attribute.required.id
    role <- Xml.attribute.optional("role")
    head <- Xml.text.required("head")
  } yield NamesList(
    entity,
    id,
    role,
    head
  )

  def readAll(directory: File, fileName: String): (String, Seq[NamesList]) = Parser.parseDo(
    From.file(directory, fileName)
      .parse(Xml.withName("names", for {
        head <- Xml.text.required("head")
        listDescriptors <- Xml.all(parser)
      } yield (head, listDescriptors)))
  )
}
