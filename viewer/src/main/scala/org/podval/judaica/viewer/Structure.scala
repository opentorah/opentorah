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


final class Structures(selectors: Selectors, xml: Elem) {

  val structures: Seq[Structure] = xml.elems("structures", "structure", required = false).map(new Structure(selectors, _))


  def byName(name: String): Option[Structure] = structures.find(_.selector.names.has(name))
}



final class Structure(selectors: Selectors, xml: Elem) extends ByName[Div] {

  val selector: Selector = selectors.byName(xml.getAttribute("type")).get


  val named: Seq[Div] = xml.elems("div").map(new Div(selectors, _))
}



final class Div(selectors: Selectors, xml: Elem) extends Named {

  override val names: Names = Names(xml)


  val structures = new Structures(selectors, xml)
}
