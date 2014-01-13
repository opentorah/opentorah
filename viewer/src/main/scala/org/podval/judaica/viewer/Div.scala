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


abstract class Div(override val selectors: Seq[Selector], val structures: Seq[Structure]) extends Structures {

  override def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)
  override def structureByName(name: String): Option[Structure] = Names.find(structures, name)

  def isNumbered: Boolean
  def asNumbered: NumberedDiv
  def asNamed: NamedDiv

  def id: String
}


final class NumberedDiv(val number: Int, selectors: Seq[Selector], structures: Seq[Structure]) extends Div(selectors, structures) {
  override def isNumbered: Boolean = true
  override def asNumbered: NumberedDiv = this
  override def asNamed: NamedDiv = throw new ClassCastException

  override def id: String = number.toString
}


final class NamedDiv(override val names: Names, selectors: Seq[Selector], structures: Seq[Structure]) extends Div(selectors, structures) with Named {
  override def isNumbered: Boolean = false
  override def asNumbered: NumberedDiv = throw new ClassCastException
  override def asNamed: NamedDiv = this

  override def id: String = names.default.name
}



object Div {

  def namedDivs(selector: NamedSelector, uncles: Seq[Selector], xml: Elem): Seq[NamedDiv] = {
    xml.elemsFilter("div").map(named(uncles, selector, _))
  }


  def numberedDivs(selector: NumberedSelector, uncles: Seq[Selector], xml: Elem): Seq[NumberedDiv] =
    xml.elemsFilter("div").zipWithIndex.map { case (xml, num) => numbered(uncles, selector, num+1, xml) }


  private[this] def numbered(uncles: Seq[Selector], selector: NumberedSelector, number: Int, xml: Elem): NumberedDiv = {
    val names = Names(xml, canBeEmpty = true)
    require(names.isEmpty, "Numbered Div can not have names")

    val nOption = xml.attributeOption("n")
    if (nOption.isDefined) {
      val nvalue = xml.getIntAttribute("n")
      require(nvalue == number, s"Div $number has attribute n set to $nvalue")
    }

    val (pathOption, selectors, structures) = body(uncles, selector, xml)

    new NumberedDiv(number, selectors, structures)
  }


  private[this] def named(uncles: Seq[Selector], selector: NamedSelector, xml: Elem): NamedDiv = {
    val names = Names(xml)

    val nOption = xml.attributeOption("n")
    require(nOption.isEmpty, "Named Div can not have attribute \"n\" set")

    val (pathOption, selectors, structures) = body(uncles, selector, xml)

    new NamedDiv(names, selectors, structures)
  }


  private def body(uncles: Seq[Selector], selector: Selector, xml: Elem): (Option[String], Seq[Selector], Seq[Structure]) = {
    val path = xml.attributeOption("path")
    val selectors: Seq[Selector] = Selector.parseSelectors(uncles, xml)
    val effectiveSelectors = selector.selectors ++ selectors
    val structures: Seq[Structure] =  Structure.parseStructures(effectiveSelectors, xml)
    // TODO complete the list of structures - even if some of them are empty!
    (path, selectors, structures)
  }
}
