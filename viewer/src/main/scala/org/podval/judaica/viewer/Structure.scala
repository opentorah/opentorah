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
  final def isNamed: Boolean = selector.isNamed

  def asNumbered: NumberedStructure
  def asNamed: NamedStructure

  def divs: Seq[Div]
  final def length: Int = divs.length // TODO for numbered, length can be supplied in the index file; parse it!

  final def divByNumber(number: Int): Option[Div] = if ((number < 1) || (number > length)) None else Some(divs(number))
  final def getDivByNumber(number: Int): Div = Exists(divByNumber(number), number.toString, "div")

  protected final def divs(xml: Elem): Seq[Elem] = xml.elemsFilter("div")
}


abstract class NamedStructure(override val selector: NamedSelector) extends Structure(selector) {
  final override def asNumbered: NumberedStructure = throw new ClassCastException
  final override def asNamed: NamedStructure = this

  override def divs: Seq[NamedDiv]

  final def divByName(name: String): Option[NamedDiv] = Names.find(divs, name)
  final def getDivByName(name: String): NamedDiv = Names.doFind(divs, name, "div")

  protected final def parseDivs(parsingFile: File, knownSelectors: Set[Selector], xml: Elem): Seq[NamedDiv] =
    withFile(parsingFile)
    { divs(xml).map(xml => new NamedDiv(this, parsingFile, knownSelectors, xml)) }
}



final class NamedParsedStructure(
  parsingFile: File,
  selector: NamedSelector,
  knownSelectors: Set[Selector],
  xml: Elem) extends NamedStructure(selector)
{
  override val divs: Seq[NamedDiv] = parseDivs(parsingFile, knownSelectors, xml)
}


final class NamedLazyStructure(
  selector: NamedSelector,
  knownSelectors: Set[Selector],
  file: File) extends NamedStructure(selector)
{
  override def divs: Seq[NamedDiv] = divs_.get
  private[this] val divs_ = LazyLoad(parseDivs(file, knownSelectors, xml))
  private[this] def xml: Elem = XmlFile.load(file, "structure")
}



abstract class NumberedStructure(override val selector: NumberedSelector) extends Structure(selector) {
  final override def asNumbered: NumberedStructure = this
  final override def asNamed: NamedStructure = throw new ClassCastException

  override def divs: Seq[NumberedDiv]

  protected final def parseDivs(parsingFile: File, knownSelectors: Set[Selector], xml: Elem): Seq[NumberedDiv] =
    withFile(parsingFile)
    { divs(xml).zipWithIndex.map { case (xml, num) => new NumberedDiv(this, parsingFile, knownSelectors, num+1, xml) } }
}


final class NumberedParsedStructure(
  parsingFile: File,
  selector: NumberedSelector,
  knownSelectors: Set[Selector],
  xml: Elem) extends NumberedStructure(selector)
{
  override val divs: Seq[NumberedDiv] = parseDivs(parsingFile, knownSelectors, xml)
}


final class NumberedLazyStructure(
  selector: NumberedSelector,
  knownSelectors: Set[Selector],
  file: File) extends NumberedStructure(selector)
{
  override def divs: Seq[NumberedDiv] = divs_.get
  private[this] val divs_ = LazyLoad(parseDivs(file, knownSelectors, metadata))
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
    // TODO verify that all structures requested by the selectors are present; some allowed structures need to be calculated...
    // TODO make sure that they are retrievable, too - for instance, week/chapter!
///    selectors.foreach(selector => Exists(structures, selector.defaultName, "structures"))
    structures
  }


  private def parseStructure(parsingFile: File, selectors: Seq[Selector], xml: Elem): Structure = {
    val selector = Names.doFind(selectors, xml.getAttribute("selector"), "selector")
    val uncles = selectors.takeWhile(_ != selector)
    // TODO the set is not big enough! Should start from the top, to accommodate old uncles...
    // TODO check that the cycles are actually prevented by all this...
    val knownSelectors = Selector.descendants(uncles.toSet)

    xml.attributeOption("file").fold {
      if (selector.isNumbered)
        new NumberedParsedStructure(parsingFile, selector.asNumbered, knownSelectors, xml)
      else
        new NamedParsedStructure(parsingFile, selector.asNamed, knownSelectors, xml)
    }{
      fileName: String =>
        val file: File = new File(parsingFile.getParentFile, fileName)
        if (selector.isNumbered)
          new NumberedLazyStructure(selector.asNumbered, knownSelectors, file)
        else
          new NamedLazyStructure(selector.asNamed, knownSelectors, file)
    }
  }
}
