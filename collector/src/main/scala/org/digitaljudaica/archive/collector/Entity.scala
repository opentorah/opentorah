package org.digitaljudaica.archive.collector

sealed trait Entity {
  def element: String
  def nameElement: String
  def listElement: String
}

object Entity {

  object Person extends Entity {
    override def element: String = "person"
    override def nameElement: String = "persName"
    override def listElement: String = "listPerson"
  }

  object Place extends Entity {
    override def element: String = "place"
    override def nameElement: String = "placeName"
    override def listElement: String = "listPlace"
  }

  object Organization extends Entity {
    override def element: String = "org"
    override def nameElement: String = "orgName"
    override def listElement: String = "listOrg"
  }

  def forElement(element: String): Option[Entity] = values.find(_.element == element)

  private val values: Set[Entity] = Set(Person, Place, Organization)
}
