/*
 * Copyright 2012-2014 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.xml.Xml.XmlOps
import scala.xml.Elem
import java.io.File


// TODO dissolve into individual companion objects and classes
object Parser {

  def parseSelectors(uncles: Seq[Selector], xmls: Elem): Seq[Selector] =
    xmls.elemsFilter("selector").foldLeft(Seq.empty[Selector])((siblings, xml) => siblings :+ parseSelector(uncles, siblings, xml))


  def parseSelector(uncles: Seq[Selector], siblings: Seq[Selector], xml: Elem): Selector = {
    val nameOption = xml.attributeOption("name")
    if (nameOption.isDefined) {
      // A reference to a previously defined Selector
      val name = nameOption.get
      val result = Names.find(uncles, name)
      require(result.isDefined, s"Selector $name not found")
      result.get
    } else {
      val names = Names(xml)
      val selectors = parseSelectors(siblings, xml)
      val isNumbered = xml.booleanAttribute("isNumbered")
      if (isNumbered) new NumberedSelector(names, selectors) else new NamedSelector(names, selectors)
    }
  }


  def parseStructures(selectors: Seq[Selector], xmls: Elem): Seq[Structure] = xmls.elemsFilter("structure").map(parseStructure(selectors, _))



  def parseStructure(selectors: Seq[Selector], xml: Elem): Structure = {
    val selectorName = xml.getAttribute("selector")
    val selectorOption = Names.find(selectors, selectorName)
    require(selectorOption.isDefined, s"No selector with name $selectorName")
    val selector = selectorOption.get
    val uncles = selectors // TODO to prevent cycles, should be the ones before our selector?
    val divXmls = xml.elemsFilter("div")

    if (selector.isNumbered) {
      val numberedSelector = selector.asNumbered
      val divs = divXmls.zipWithIndex.map { case (xml, num) => numberedDiv(uncles, numberedSelector, num+1, xml) }
      new NumberedStructure(numberedSelector, divs)
    } else {
      val namedSelector = selector.asNamed
      val divs = divXmls.map(namedDiv(uncles, namedSelector, _))
      new NamedStructure(namedSelector, divs)
    }
  }


  def numberedDiv(uncles: Seq[Selector], selector: NumberedSelector, number: Int, xml: Elem): NumberedDiv = {
    val names = Names(xml, canBeEmpty = true)
    require(names.isEmpty, "Numbered Div can not have names")

    val nOption = xml.attributeOption("n")
    if (nOption.isDefined) {
      val nvalue = xml.getIntAttribute("n")
      require(nvalue == number, s"Div $number has attribute n set to $nvalue")
    }

    val (pathOption, selectors, structures) = divBody(uncles, selector, xml)

    new NumberedDiv(number, selectors, structures)
  }


  def namedDiv(uncles: Seq[Selector], selector: NamedSelector, xml: Elem): NamedDiv = {
    val names = Names(xml)

    val nOption = xml.attributeOption("n")
    require(nOption.isEmpty, "Named Div can not have attribute \"n\" set")

    val (pathOption, selectors, structures) = divBody(uncles, selector, xml)

    new NamedDiv(names, selectors, structures)
  }


  def divBody(uncles: Seq[Selector], selector: Selector, xml: Elem): (Option[String], Seq[Selector], Seq[Structure]) = {
    val path = xml.attributeOption("path")
    val selectors: Seq[Selector] = parseSelectors(uncles, xml)
    val effectiveSelectors = selector.selectors ++ selectors
    val structures: Seq[Structure] =  parseStructures(effectiveSelectors, xml)
    (path, selectors, structures)
  }
}
