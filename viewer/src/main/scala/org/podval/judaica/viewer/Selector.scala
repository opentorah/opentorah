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


final class Selectors private(override val named: Seq[Selector]) extends ByName[Selector]



object Selectors {

  def apply(xml: Elem): Selectors = {
    val xmls = xml.elems("selectors", "selector")
    val names2xml: Map[Names, Elem] = xmls.map(xml => Names(xml) -> xml).toMap
    val names = names2xml.keySet

    val names2selectors: Map[Names, Set[Names]] = names2xml.mapValues { selectorXml: Elem =>
      selectorXml.elems("selectors", "selector", required = false).toSet.map { subselectorXml: Elem =>
        val name = subselectorXml.getAttribute("name")
        names.find(_.has(name)).get
      }
    }

    val ordered: Seq[Names] = Orderer.order(names2selectors).right.get


    def build(acc: Seq[Selector], names2selector: Map[Names, Selector], left: Seq[Names]): Seq[Selector] =
      if (left.isEmpty) acc else {
        val names = left.head
        val selectors: Set[Selector] = names2selectors(names).map(names2selector)
        val selector: Selector = new Selector(names, selectors, names2xml(names))
        build(acc :+ selector, names2selector.updated(selector.names, selector), left.tail)
      }


    val selectors: Seq[Selector] = build(Seq.empty, Map.empty, ordered)

    new Selectors(selectors)
  }
}



final class Selector(override val names: Names, val selectors: Set[Selector], xml: Elem) extends Named {

  val isNumeric: Boolean = xml.booleanAttribute("isNumeric")
}
