/*
 *  Copyright 2014 Leonid Dubinsky <dub@podval.org>.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.judaica.viewer

import org.podval.judaica.xml.Xml.Ops
import org.podval.judaica.xml.XmlFile
import ParseException.withFile

import scala.xml.Elem
import java.io.File


abstract class Structure(val selector: Selector) extends Named {
  final override def names = selector.names
  final def isNumbered: Boolean = selector.isNumbered

  def asNumbered: NumberedStructure
  def asNamed: NamedStructure

  def divs: Seq[Div]
  final def length: Int = divs.length

  final def divByNumber(number: Int): Option[Div] = if ((number < 1) || (number > length)) None else Some(divs(number))
  final def getDivByNumber(number: Int): Div = Exists(divByNumber(number), number.toString, "div")

  protected final def divs(xml: Elem): Seq[Elem] = xml.elemsFilter("div")
}


abstract class NamedStructure(override val selector: NamedSelector) extends Structure(selector) {
  final override def asNumbered: NumberedStructure = throw new ClassCastException
  final override def asNamed: NamedStructure = this

  override def divs: Seq[NamedDiv]

  final def divByName(name: String): Option[NamedDiv] = Names.find(divs, name)
  final def getDivByName(name: String): NamedDiv = Exists(divs, name, "div")

  protected final def parseDivs(parsingFile: File, uncles: Seq[Selector], xml: Elem): Seq[NamedDiv] =
    withFile(parsingFile)
    { divs(xml).map(xml => new NamedDiv(this, parsingFile, uncles, xml)) }
}



final class NamedParsedStructure(
  parsingFile: File,
  selector: NamedSelector,
  uncles: Seq[Selector],
  xml: Elem) extends NamedStructure(selector)
{
  override val divs: Seq[NamedDiv] = parseDivs(parsingFile, uncles, xml)
}


final class NamedLazyStructure(
  selector: NamedSelector,
  uncles: Seq[Selector],
  file: File) extends NamedStructure(selector)
{
  override def divs: Seq[NamedDiv] = divs_.get
  private[this] val divs_ = LazyLoad(parseDivs(file, uncles, xml))
  private[this] def xml: Elem = XmlFile.load(file, "structure")
}



abstract class NumberedStructure(override val selector: NumberedSelector) extends Structure(selector) {
  final override def asNumbered: NumberedStructure = this
  final override def asNamed: NamedStructure = throw new ClassCastException

  override def divs: Seq[NumberedDiv]

  protected final def parseDivs(parsingFile: File, uncles: Seq[Selector], xml: Elem): Seq[NumberedDiv] =
    withFile(parsingFile)
    { divs(xml).zipWithIndex.map { case (xml, num) => new NumberedDiv(this, parsingFile, uncles, num+1, xml) } }
}


final class NumberedParsedStructure(
  parsingFile: File,
  selector: NumberedSelector,
  uncles: Seq[Selector],
  xml: Elem) extends NumberedStructure(selector)
{
  override val divs: Seq[NumberedDiv] = parseDivs(parsingFile, uncles, xml)
}


final class NumberedLazyStructure(
  selector: NumberedSelector,
  uncles: Seq[Selector],
  file: File) extends NumberedStructure(selector)
{
  override def divs: Seq[NumberedDiv] = divs_.get
  private[this] val divs_ = LazyLoad(parseDivs(file, uncles, metadata))
  private[this] def metadata: Elem = XmlFile.load(file, "structure")
}



trait Structures extends Selectors {
  def structures: Seq[Structure]
  def structureByName(name: String): Option[Structure]
  def getStructureByName(name: String): Structure = Exists(structureByName(name), name, "structure")
}



object Structure {

  def parseStructures(parsingFile: File, selectors: Seq[Selector], xmls: Elem): Seq[Structure] = {
    val structures = xmls.elemsFilter("structure").map(parseStructure(parsingFile, selectors, _))
    // TODO verify that all structures requested by the selectors are present; some allowed structers need to be calculated...
    // TODO make sure that they are retrievable, too - for instance, week/chapter!
///    selectors.foreach(selector => Exists(structures, selector.defaultName, "structures"))
    structures
  }


  private def parseStructure(parsingFile: File, selectors: Seq[Selector], xml: Elem): Structure = {
    val selector = Exists(selectors, xml.getAttribute("selector"), "selector")
    val uncles = selectors.takeWhile(_ != selector)

    val fileOption = xml.attributeOption("file")
    if (fileOption.isEmpty) {
      if (selector.isNumbered) {
        new NumberedParsedStructure(parsingFile, selector.asNumbered, uncles, xml)
      } else {
        new NamedParsedStructure(parsingFile, selector.asNamed, uncles, xml)
      }
    } else {
      val file = new File(parsingFile.getParentFile, fileOption.get)
      if (selector.isNumbered) {
        new NumberedLazyStructure(selector.asNumbered, uncles, file)
      } else {
        new NamedLazyStructure(selector.asNamed, uncles, file)
      }
    }
  }
}
