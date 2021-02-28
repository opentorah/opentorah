package org.opentorah.texts.tanach

trait Prophets extends Nach

object Prophets {
  val all: Seq[Prophets] = EarlyProphets.all ++ LateProphets.all

  def forName(name: String): Prophets = Tanach.getForName(name).asInstanceOf[Prophets]
}
