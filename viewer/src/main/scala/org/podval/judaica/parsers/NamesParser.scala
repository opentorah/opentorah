/*
 *  Copyright 2014-2018 Leonid Dubinsky <dub@podval.org>.
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
package org.podval.judaica.parsers

import org.podval.judaica.viewer.{Exists, Name, Names}

import scala.xml.Elem
import Xml.Ops

object NamesParser {

  private def name(xml: Elem) = new Name(
    xml.getAttribute("name"),
    xml.getAttribute("lang"),
    xml.booleanAttribute("isTransliterated")
  )

  //  def apply(name: String, xml: Elem, canBeEmpty: Boolean = false): Names = new Names(Some(name), xml, canBeEmpty)

  def names(xml: Elem): Names = {
    val names: Seq[Name] = Exists(xml.elemsFilter("name").map(name), "names")
    new Names(names)
  }
}
