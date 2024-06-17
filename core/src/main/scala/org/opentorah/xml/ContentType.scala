package org.opentorah.xml

enum ContentType(
  val elementsAllowed: Boolean,
  val charactersAllowed: Boolean
) derives CanEqual:
  case Empty      extends ContentType(elementsAllowed = false, charactersAllowed = false)
  case Characters extends ContentType(elementsAllowed = false, charactersAllowed = true )
  case Elements   extends ContentType(elementsAllowed = true , charactersAllowed = false)
  case Mixed      extends ContentType(elementsAllowed = true , charactersAllowed = true )
  