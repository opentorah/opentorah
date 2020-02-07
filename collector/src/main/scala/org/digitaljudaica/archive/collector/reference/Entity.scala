package org.digitaljudaica.archive.collector.reference

sealed trait Entity {
  def element: String
  def nameElement: String
  def listElement: String
}

object Entity {

  case object Person extends Entity {
    override def element: String = "person"
    override def nameElement: String = "persName"
    override def listElement: String = "listPerson"
  }

  case object Place extends Entity {
    override def element: String = "place"
    override def nameElement: String = "placeName"
    override def listElement: String = "listPlace"
  }

  case object Organization extends Entity {
    override def element: String = "org"
    override def nameElement: String = "orgName"
    override def listElement: String = "listOrg"
  }

  val values: Seq[Entity] = Seq(Person, Place, Organization)

  def forElement(element: String): Option[Entity] = values.find(_.element == element)
  def forName(element: String): Option[Entity] = values.find(_.nameElement == element)
  def forList(element: String): Option[Entity] = values.find(_.listElement == element)
}
