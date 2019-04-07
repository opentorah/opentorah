package org.podval.archive19kislev.collector

import java.io.File

import scala.xml.Elem
import Xml.Ops

abstract class DocumentLike(directory: File, val fileName: String) {
  override def toString: String = fileName

  def url: String

  protected final val tei: Elem = Xml.open(Xml.load(directory, fileName), "TEI")
  protected final val teiHeader: Elem = tei.oneChild("teiHeader")
  protected final val fileDesc: Elem = teiHeader.oneChild("fileDesc")
  protected final val profileDesc: Elem = teiHeader.oneChild("profileDesc")
  protected final val body: Elem = tei.oneChild("text").oneChild("body")

  protected final def teiDescendants(what: String): Seq[Elem] = tei.descendants(what)

  protected final def names(elems: Seq[Elem]): Seq[Name] = elems.map(elem => Name(this, elem))

  def persNames: Seq[Name]
  def placeNames: Seq[Name]
  def orgNames: Seq[Name]

  final def names: Seq[Name] = persNames ++ placeNames ++ orgNames

  final def write(xml: Elem): Unit = Xml.write(directory, fileName, xml)
}
