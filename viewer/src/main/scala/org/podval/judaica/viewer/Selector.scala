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


abstract class Selector(knownSelectors: Set[Selector], xml: Elem) extends Named with Selectors {
  def isNumbered: Boolean
  final def isNamed: Boolean = !isNumbered 
  def asNumbered: NumberedSelector
  def asNamed: NamedSelector

  final override val names = Names(xml)
  final override val selectors = Selectors.parse(knownSelectors, xml)

  final def isTerminal: Boolean = selectors.isEmpty
}


final class NumberedSelector(knownSelectors: Set[Selector], xml: Elem) extends Selector(knownSelectors, xml) {
  override def isNumbered: Boolean = true
  override def asNumbered: NumberedSelector = this
  override def asNamed: NamedSelector = throw new ClassCastException
}


final class NamedSelector(knownSelectors: Set[Selector], xml: Elem) extends Selector(knownSelectors, xml) {
  override def isNumbered: Boolean = false
  override def asNumbered: NumberedSelector = throw new ClassCastException
  override def asNamed: NamedSelector = this
}


trait Selectors {

  type Format = Seq[Selector]


  def selectors: Seq[Selector]


  final def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)


  final def getSelectorByName(name: String): Selector = Names.doFind(selectors, name, "selector")


  final def dominantSelector: Selector = selectors.head


  final def dominantFormat: Format =
    if (selectors.isEmpty) Nil else dominantSelector +: dominantSelector.dominantFormat


  final def formats: Seq[Format] =
    if (selectors.isEmpty) Seq(Nil) else
    selectors.flatMap(selector => selector.formats.map (selector +: _))


  final def parseFormat(formatOption: Option[String]): Format = formatOption.fold(dominantFormat)(parseFormat)


  final def parseFormat(format: String): Format =
    Parse.sequence[String, Selectors, Selector](_.getSelectorByName(_)) ((selectors, selector) => selector) (this, format.split("/"))
}



object Selectors {

  def parse(knownSelectors: Set[Selector], xml: Elem): Seq[Selector] =
    Parse.sequence[Elem, Set[Selector], Selector](parseSelector)(descendantsOfOne)(knownSelectors, xml.elemsFilter("selector"))


  private def parseSelector(knownSelectors: Set[Selector], xml: Elem): Selector = {
    def newSelector =
      if (xml.booleanAttribute("isNumbered"))
        new NumberedSelector(knownSelectors, xml)
      else
        new NamedSelector(knownSelectors, xml)

    def referenceToKnownSelector(name: String) = Names.doFind(knownSelectors, name, "selector")

    xml.attributeOption("name").fold(newSelector)(referenceToKnownSelector)
  }


  def descendants(next: Set[Selector]): Set[Selector] = descendants(Set.empty, next)


  private def descendantsOfOne(result: Set[Selector], next: Selector): Set[Selector] = descendants(result, Set(next))


  private def descendants(result: Set[Selector], next: Set[Selector]): Set[Selector] = {
    val add = next -- result
    if (add.isEmpty) result else {
      val children: Set[Selector] = add.flatMap(_.selectors)
      descendants(result ++ next, children)
    }
  }
}
