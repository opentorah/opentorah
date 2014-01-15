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


abstract class Selector(known: Set[Selector], xml: Elem) extends Named with Selectors {
  def isNumbered: Boolean
  def asNumbered: NumberedSelector
  def asNamed: NamedSelector

  override val names = Names(xml)
  override val selectors = Selector.parseSelectors(known, xml)
  override def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)
}


final class NumberedSelector(known: Set[Selector], xml: Elem) extends Selector(known, xml) {
  override def isNumbered: Boolean = true
  override def asNumbered: NumberedSelector = this
  override def asNamed: NamedSelector = throw new ClassCastException
}


final class NamedSelector(known: Set[Selector], xml: Elem) extends Selector(known, xml) {
  override def isNumbered: Boolean = false
  override def asNumbered: NumberedSelector = throw new ClassCastException
  override def asNamed: NamedSelector = this
}


trait Selectors {

  def selectors: Seq[Selector]


  def selectorByName(name: String): Option[Selector]


  def deepStructures: Seq[Seq[Selector]] =
    if (selectors.isEmpty) Seq(Nil) else
    selectors.flatMap(selector => selector.deepStructures.map (ds => selector +: ds))
}



object Selector {

  // TODO make readable :)
  def parseSelectors(known: Set[Selector], xmls: Elem): Seq[Selector] =
    xmls.elemsFilter("selector").foldLeft((known, Seq.empty[Selector])) {
      case ((known, siblings), xml) =>
        val selector = parseSelector(known, xml)
        (descendants(known, Set(selector)) , siblings :+ selector)
    }._2


  private def parseSelector(known: Set[Selector], xml: Elem): Selector = {
    val nameOption = xml.attributeOption("name")
    if (nameOption.isDefined) {
      // A reference to a previously defined Selector
      Exists(known.toSeq, nameOption.get, "selector") // TODO remove toSeq...
    } else {
      if (xml.booleanAttribute("isNumbered"))
        new NumberedSelector(known, xml)
      else
        new NamedSelector(known, xml)
    }
  }


  def descendants(next: Set[Selector]): Set[Selector] = descendants(Set.empty, next)


  def descendants(result: Set[Selector], next: Set[Selector]): Set[Selector] = {
    val add = next -- result
    if (add.isEmpty) result else {
      val children: Set[Selector] = add.flatMap(_.selectors)
      descendants(result ++ next, children)
    }
  }
}
