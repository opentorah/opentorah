package org.opentorah.texts.tanach

trait LateProphets extends Prophets

object LateProphets {

  case object Isaiah extends LateProphets
  case object Jeremiah extends LateProphets
  case object Ezekiel extends LateProphets

  val all: Seq[Prophets] = Seq(Isaiah, Jeremiah, Ezekiel) ++ TreiAsar.all
}
