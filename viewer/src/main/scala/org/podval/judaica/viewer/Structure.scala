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

import org.podval.judaica.xml.Xml.XmlOps

import scala.xml.Elem


final class Structures private(val structures: Seq[Structure]) {

  def byName(name: String): Option[Structure] = structures.find(_.selector.names.has(name))
}



abstract class Structure(val selector: Selector, val divs: Seq[Div])


final class NamedStructure(override val selector: NamedSelector, override val divs: Seq[NamedDiv]) extends Structure(selector, divs) with ByName[NamedDiv] {

  override def named = divs
}


final class NumberedStructure(override val selector: NumberedSelector, override val divs: Seq[NumberedDiv]) extends Structure(selector, divs) {  // TODO something with known length, not Seq...
}



abstract class Div(val structures: Structures)

final class NumberedDiv(val number: Int, structures: Structures) extends Div(structures)

final class NamedDiv(override val names: Names, structures: Structures) extends Div(structures) with Named



object Structures {

  def apply(selectors: Selectors, xml: Elem): Structures = parseStructures(selectors, xml)


  private def parseStructures(selectors: Selectors, xml: Elem): Structures = {
    val structures = xml.elemsFilter("selector").map(parseStructure(selectors, _))
    new Structures(structures)
  }


  private def parseStructure(selectors: Selectors, xml: Elem): Structure = {
    val selector: Selector = selectors.byName(xml.getAttribute("name")).get

    def parseDivs[T <: Div](f: (Selectors, Elem) => T): Seq[T] = xml.elems("div").map(f(selector.selectors, _))

    if (selector.isNumbered) {
      new NumberedStructure(selector.asNumbered, parseDivs(parseNumberedDiv))
    } else {
      new NamedStructure(selector.asNamed, parseDivs(parseNamedDiv))
    }
  }


  private def parseNamedDiv(selectors: Selectors, xml: Elem): NamedDiv = {
    val structures = parseStructures(selectors, xml)
    val names = Names(xml)
    new NamedDiv(names, structures)
  }


  private def parseNumberedDiv(selectors: Selectors, xml: Elem): NumberedDiv = {
    val structures = parseStructures(selectors, xml)
    val n = xml.getIntAttribute("n")
    new NumberedDiv(n, structures)
  }
}
