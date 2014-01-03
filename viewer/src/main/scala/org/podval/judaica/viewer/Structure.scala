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

import org.podval.judaica.xml.Xml.{oneChild, elems, getAttribute}

import scala.xml.Elem


final class Structures(xml: Elem) {

  val structures: Seq[Structure] = elems(oneChild(xml, "structures"), "structure").map(new Structure(_))


  def find(name: String): Option[Structure] = structures.find(_.type_ == name)
}



final class Structure(xml: Elem) {

  val type_ : String = getAttribute(xml, "type")


  val divs: Seq[Div] = elems(xml, "div").map { div => new Div(Names(div)) } // TODO why don't () work? _?
}
