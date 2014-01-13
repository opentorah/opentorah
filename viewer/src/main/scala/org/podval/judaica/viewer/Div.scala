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


abstract class Div(parsingFile: File, uncles: Seq[Selector], selector: Selector, xml: Elem) extends Structures {

  // TODO parse the Path
/////  val path = xml.attributeOption("path")
  override val selectors: Seq[Selector] = Selector.parseSelectors(uncles, xml)
  val effectiveSelectors = selector.selectors ++ selectors
  override val structures: Seq[Structure] =  Structure.parseStructures(parsingFile, effectiveSelectors, xml)
  // TODO complete the list of structures - even if some of them are empty!


  override def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)
  override def structureByName(name: String): Option[Structure] = Names.find(structures, name)

  def isNumbered: Boolean
  def asNumbered: NumberedDiv
  def asNamed: NamedDiv

  def id: String
}


final class NumberedDiv(parsingFile: File, uncles: Seq[Selector], selector: NumberedSelector, val number: Int, xml: Elem)
  extends Div(parsingFile, uncles, selector, xml)
{
  {
    val nOption = xml.attributeOption("n")
    if (nOption.isDefined) {
      val nvalue = xml.getIntAttribute("n")
      require(nvalue == number, s"Div $number has attribute n set to $nvalue")
    }
  }


  override def isNumbered: Boolean = true
  override def asNumbered: NumberedDiv = this
  override def asNamed: NamedDiv = throw new ClassCastException

  override def id: String = number.toString
}


final class NamedDiv(parsingFile: File, uncles: Seq[Selector], selector: NamedSelector, xml: Elem)
  extends Div(parsingFile, uncles, selector, xml) with Named
{
  override val names = Names(xml)

  override def isNumbered: Boolean = false
  override def asNumbered: NumberedDiv = throw new ClassCastException
  override def asNamed: NamedDiv = this

  override def id: String = names.default.name
}



object Div {

  def namedDivs(parsingFile: File, selector: NamedSelector, uncles: Seq[Selector], xml: Elem): Seq[NamedDiv] =
    xml.elemsFilter("div").map(xml => new NamedDiv(parsingFile, uncles, selector, xml))


  def numberedDivs(parsingFile: File, selector: NumberedSelector, uncles: Seq[Selector], xml: Elem): Seq[NumberedDiv] =
    xml.elemsFilter("div").zipWithIndex.map { case (xml, num) => new NumberedDiv(parsingFile, uncles, selector, num+1, xml) }
}
