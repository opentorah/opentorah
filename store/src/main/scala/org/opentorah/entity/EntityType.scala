package org.opentorah.entity

sealed trait EntityType {
  def element: String
  def nameElement: String
  def listElement: String
}

object EntityType {

  case object Person extends EntityType {
    override def element: String = "person"
    override def nameElement: String = "persName"
    override def listElement: String = "listPerson"
  }

  case object Place extends EntityType {
    override def element: String = "place"
    override def nameElement: String = "placeName"
    override def listElement: String = "listPlace"
  }

  case object Organization extends EntityType {
    override def element: String = "org"
    override def nameElement: String = "orgName"
    override def listElement: String = "listOrg"
  }

  val values: Seq[EntityType] = Seq(Person, Place, Organization)

  def forElement(element: String): Option[EntityType] = values.find(_.element == element)
  def forName(element: String): Option[EntityType] = values.find(_.nameElement == element)
  def forList(element: String): Option[EntityType] = values.find(_.listElement == element)
}
