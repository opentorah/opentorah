package org.opentorah.texts.tanach

sealed trait EarlyProphets extends Prophets

object EarlyProphets {

  case object Joshua extends EarlyProphets
  case object Judges extends EarlyProphets
  case object SamuelI extends EarlyProphets {
    override def name: String = "I Samuel"
  }
  case object SamuelII extends EarlyProphets {
    override def name: String = "II Samuel"
  }
  case object KingsI extends EarlyProphets {
    override def name: String = "I Kings"
  }
  case object KingsII extends EarlyProphets {
    override def name: String = "II Kings"
  }

  val all: Seq[Prophets] = Seq(Joshua, Judges, SamuelI, SamuelII, KingsI, KingsII)
}
