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
import scala.xml.Elem
import java.io.File
import org.podval.judaica.xml.XmlFile


abstract class Structure(val selector: Selector) extends Named {
  final override def names = selector.names
  final def isNumbered: Boolean = selector.isNumbered

  def asNumbered: NumberedStructure
  def asNamed: NamedStructure

  def divs: Seq[Div]
  final def length: Int = divs.length

  final def divByNumber(number: Int): Option[Div] = if ((number < 1) || (number > length)) None else Some(divs(number))
}


abstract class NamedStructure(override val selector: NamedSelector) extends Structure(selector) {
  final override def asNumbered: NumberedStructure = throw new ClassCastException
  final override def asNamed: NamedStructure = this

  override def divs: Seq[NamedDiv]

  final def divByName(name: String): Option[NamedDiv] = Names.find(divs, name)
}


// TODO something with known length, not Seq...

final class NamedParsedStructure(
  selector: NamedSelector,
  uncles: Seq[Selector],
  xml: Elem) extends NamedStructure(selector)
{
  override val divs: Seq[NamedDiv] = Div.namedDivs(selector, uncles, xml)
}


final class NamedLazyStructure(
  selector: NamedSelector,
  uncles: Seq[Selector],
  file: File) extends NamedStructure(selector)
{
  override def divs: Seq[NamedDiv] = divs_.get
  private[this] val divs_ : Soft[Seq[NamedDiv]] = Soft(Div.namedDivs(selector, uncles, xml))
  private[this] def xml: Elem = XmlFile.load(file, "structure")
}



abstract class NumberedStructure(override val selector: NumberedSelector) extends Structure(selector) {
  final override def asNumbered: NumberedStructure = this
  final override def asNamed: NamedStructure = throw new ClassCastException

  override def divs: Seq[NumberedDiv]
}


final class NumberedParsedStructure(
  selector: NumberedSelector,
  uncles: Seq[Selector],
  xml: Elem) extends NumberedStructure(selector)
{
  override val divs: Seq[NumberedDiv] = Div.numberedDivs(selector, uncles, xml)
}


final class NumberedLazyStructure(
  selector: NumberedSelector,
  uncles: Seq[Selector],
  file: File) extends NumberedStructure(selector)
{
  override def divs: Seq[NumberedDiv] = divs_.get
  private[this] val divs_ : Soft[Seq[NumberedDiv]] = Soft(Div.numberedDivs(selector, uncles, metadata))
  private[this] def metadata: Elem = XmlFile.load(file, "structure")
}



trait Structures extends Selectors {
  def structures: Seq[Structure]
  def structureByName(name: String): Option[Structure]
}



object Structure {

  def parseStructures(selectors: Seq[Selector], xmls: Elem): Seq[Structure] = xmls.elemsFilter("structure").map(parseStructure(selectors, _))


  private def parseStructure(selectors: Seq[Selector], xml: Elem): Structure = {
    val selectorName = xml.getAttribute("selector")
    val selector = Exists(Names.find(selectors, selectorName), selectorName, "selector")
    val uncles = selectors.takeWhile(_ != selector)

    // TODO detect "file" attribute and load accordingly!!!

    if (selector.isNumbered) {
      new NumberedParsedStructure(selector.asNumbered, uncles, xml)
    } else {
      new NamedParsedStructure(selector.asNamed, uncles, xml)
    }
  }
}
