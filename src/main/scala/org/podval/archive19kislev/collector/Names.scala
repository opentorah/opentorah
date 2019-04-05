package org.podval.archive19kislev.collector

import java.io.File

import scala.xml.Elem
import Xml.Ops

class Names(directory: File, fileName: String) {
  private val tei: Elem = Xml.load(directory, fileName)

  private val personElems: Seq[Elem] = tei.descendants("person")
  private val placeElems: Seq[Elem] = tei.descendants("place")
  private val orgElems: Seq[Elem] = tei.descendants("org")

  val people: Seq[Named] = personElems.map(Named.apply(_, "persName"))
  val places: Seq[Named] = placeElems.map(Named.apply(_, "placeName"))
  val organizations: Seq[Named] = orgElems.map(Named.apply(_, "orgName"))

  private def things: Seq[Named] = people ++ places ++ organizations

  private val persNames: Set[Name] = names("persName", personElems)
  private val placeNames: Set[Name] = names("placeName", placeElems)
  private val orgNames: Set[Name] = names("orgName", orgElems)
  val references: Set[Name] = persNames ++ placeNames ++ orgNames

  private def names(what: String, namedElems: Seq[Elem]): Set[Name] =
    (tei.descendants(what).toSet -- namedElems.flatMap(_.elemsFilter(what)).toSet).map(Name.apply)

  def find(id: String): Option[Named] = things.find(_.id.contains(id))
}


