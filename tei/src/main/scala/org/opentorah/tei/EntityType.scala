package org.opentorah.tei

enum EntityType(
  val element: String,
  val nameElement: String,
  val listElement: String
) derives CanEqual:
  case Person       extends EntityType("person",  "persName", "listPerson")
  case Place        extends EntityType("place" , "placeName", "listPlace" )
  case Organization extends EntityType("org"   ,   "orgName", "listOrg"   )

object EntityType:
  def isName(elementName: String): Boolean = values.exists(_.nameElement == elementName)
