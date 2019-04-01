package org.podval.archive19kislev.collector

import java.io.File

import scala.xml.Elem
import Xml.Ops

class Names(directory: File, fileName: String) {
  private val elem: Elem = Xml.load(directory, fileName)

  val people: Seq[Named] = elem.descendants("person").map(Named.apply(_, "persName"))
  val places: Seq[Named] = elem.descendants("place").map(Named.apply(_, "placeName"))
  val organizations: Seq[Named] = elem.descendants("org").map(Named.apply(_, "orgName"))

  private def things: Seq[Named] = people ++ places ++ organizations

  def find(id: String): Option[Named] = things.find(_.id.contains(id))
}
