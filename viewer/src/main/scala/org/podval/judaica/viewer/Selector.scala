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
  final override val selectors = Selector.parseSelectors(knownSelectors, xml)
  final override def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)
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
  def selectorByName(name: String): Option[Selector]
  def getSelectorByName(name: String): Selector = Exists(selectorByName(name), name, "selector")
  final def defaultSelector: Selector = selectors.head


  final def defaultFormat: Format =
    if (selectors.isEmpty) Nil else defaultSelector +: defaultSelector.defaultFormat


  final def formats: Seq[Format] =
    if (selectors.isEmpty) Seq(Nil) else
    selectors.flatMap(selector => selector.formats.map (selector +: _))


  final def parseFormat(formatOption: Option[String]): Format = formatOption.fold(defaultFormat)(parseFormat)


  final def parseFormat(format: String): Format = {
    def parseFormat(selectors: Selectors, names: Seq[String]): Format = names match {
      case Nil => Nil
      case name :: names =>
        val selector = selectors.getSelectorByName(name)
        selector +: parseFormat(selector, names)
    }

    parseFormat(this, format.split("/"))
  }
}



object Selector {

  // TODO make more readable?
  def parseSelectors(knownSelectors: Set[Selector], xml: Elem): Seq[Selector] = {
    def parseSelectors(knownSelectors: Set[Selector], xmls: Seq[Elem]): Seq[Selector] = xmls match {
      case Nil => Nil
      case xml :: xmls =>
        val selector = parseSelector(knownSelectors, xml)
        val newKnowSelectors = descendants(knownSelectors, Set(selector))
        selector +: parseSelectors(newKnowSelectors, xmls)
    }

    parseSelectors(knownSelectors, xml.elemsFilter("selector"))
  }


  private def parseSelector(knownSelectors: Set[Selector], xml: Elem): Selector = {
    xml.attributeOption("name").fold {
      if (xml.booleanAttribute("isNumbered"))
        new NumberedSelector(knownSelectors, xml)
      else
        new NamedSelector(knownSelectors, xml)
    }{
      // A reference to a previously defined Selector
      Names.doFind(knownSelectors.toSeq, _, "selector") // TODO remove toSeq...
    }
  }


  def descendants(next: Set[Selector]): Set[Selector] = descendants(Set.empty, next)


  private def descendants(result: Set[Selector], next: Set[Selector]): Set[Selector] = {
    val add = next -- result
    if (add.isEmpty) result else {
      val children: Set[Selector] = add.flatMap(_.selectors)
      descendants(result ++ next, children)
    }
  }
}
