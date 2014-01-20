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
import Selector.ParsingContext
import ParseException.withMetadataFile

import scala.xml.Elem

import java.io.File


abstract class Structure(val selector: Selector, xml: Elem) extends Named with Ordering[Div] {

  final override def names = selector.names


  final def isNumbered: Boolean = selector.isNumbered


  final def isNamed: Boolean = selector.isNamed


  def asNumbered: NumberedStructure


  def asNamed: NamedStructure


  final def isTerminal: Boolean = selector.isTerminal


  def divs: Seq[Div]


  final def length: Int = lengthOption.getOrElse(divs.length)


  private[this] val lengthOption: Option[Int] = xml.intAttributeOption("length")


  final override def compare(x: Div, y: Div): Int = {
    require(x.structure == this && y.structure == this)
    require(divs.contains(x) && divs.contains(y))
    divs.indexOf(x) - divs.indexOf(y)
  }


  final def divByNumber(number: Int): Option[Div] = if ((number < 1) || (number > length)) None else Some(divs(number))


  final def getDivByNumber(number: Int): Div = Exists(divByNumber(number), number.toString, "div")


  protected final def open(xml: Elem): Elem = {
    val result = xml.oneChild("structure")
    // TODO check that this is the correct structure :)
    result
  }


  // TODO check that named or non-dominant structure is fully defined
  protected final def checkLength[T <: Div](divs: Seq[T]): Seq[T] =
    if (lengthOption.isDefined && lengthOption.get != divs.length) throw new ViewerException(s"Wrong length") else divs


  protected final def divs(xml: Elem): Seq[Elem] = xml.elemsFilter("div")
}



abstract class NamedStructure(override val selector: NamedSelector, xml: Elem) extends Structure(selector, xml) {

  final override def asNumbered: NumberedStructure = throw new ClassCastException


  final override def asNamed: NamedStructure = this


  override def divs: Seq[NamedDiv]


  final def divByName(name: String): Option[NamedDiv] = Names.find(divs, name)


  final def getDivByName(name: String): NamedDiv = Names.doFind(divs, name, "div")


  protected final def parseDivs(context: ParsingContext, xml: Elem): Seq[NamedDiv] =
    checkLength(divs(xml).map(xml => NamedDiv(context, this, xml)))
}



final class NamedParsedStructure(context: ParsingContext, selector: NamedSelector, xml: Elem)
  extends NamedStructure(selector, xml)
{
  override val divs: Seq[NamedDiv] = parseDivs(context, xml)
}



final class NamedLazyStructure(context: ParsingContext, selector: NamedSelector, xml: Elem)
  extends NamedStructure(selector, xml)
{
  override def divs: Seq[NamedDiv] = divs_.get


  private[this] val divs_ = LazyLoad(withMetadataFile(context.parsingFile)(xml => parseDivs(context, open(xml))))
}



abstract class NumberedStructure(override val selector: NumberedSelector, xml: Elem) extends Structure(selector, xml) {

  final override def asNumbered: NumberedStructure = this


  final override def asNamed: NamedStructure = throw new ClassCastException


  override def divs: Seq[NumberedDiv]


  protected final def parseDivs(context: ParsingContext, xml: Elem): Seq[NumberedDiv] =
    checkLength(divs(xml).zipWithIndex.map { case (xml, num) => NumberedDiv(context, this, num+1, xml) })
}



final class NumberedParsedStructure(context: ParsingContext, selector: NumberedSelector, xml: Elem)
  extends NumberedStructure(selector, xml)
{

  override val divs: Seq[NumberedDiv] = parseDivs(context, xml)
}



final class NumberedLazyStructure(context: ParsingContext, selector: NumberedSelector, xml: Elem)
  extends NumberedStructure(selector, xml)
{
  override def divs: Seq[NumberedDiv] = divs_.get


  private[this] val divs_ = LazyLoad(withMetadataFile(context.parsingFile)(xml => parseDivs(context, open(xml))))
}



trait Structures extends Selectors {

  def structures: Map[Selector, Structure]


  final def getStructure(selector: Selector): Structure = structures(selector)
}
