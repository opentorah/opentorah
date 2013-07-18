/*
 *  Copyright 2011-2013 Leonid Dubinsky <dub@podval.org>.
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
 * under the License.
 */

package org.podval.judaica.structure

import org.podval.judaica.xml.Xml

import scala.xml.Elem


abstract class Element {

  def recognizes(elem: Elem): Boolean


  def display(elem: Elem): Seq[Elem]
}



abstract class DivElement(val type_ : String, children: Element*) extends Element {

  final override def recognizes(elem: Elem): Boolean = (elem.label == "div") && (Xml.getAttribute(elem, "type") == type_)


  final override def display(elem: Elem): Seq[Elem] = Seq(displayDiv(elem))


  final def displayDiv(elem: Elem): Elem = {
    val name: Option[Elem] = if (isNameNumeric) {
      displayName(Xml.getAttribute(elem, "n")).map(n => <span class={type_ + "number"}>{n}</span>)
    } else {
      Some(<span class={type_ + "name"}></span>)
    }

    if (isEmpty) {
      name.get
    } else {
      val contents: Seq[Elem] = Xml.elems(elem).flatMap(e => findChild(e).display(e))
      val result = name ++ contents ++ suffix

      <div class={type_}>{result}</div>
    }
  }


  private[this] def isEmpty: Boolean = children.isEmpty


  private[this] def findChild(elem: Elem): Element = children.find(_.recognizes(elem)).get


  def isNameNumeric: Boolean


  def displayName(n: String): Option[String]


  def suffix: Seq[Elem] = Seq.empty
}



abstract class NonDivElement(final val label: String) extends Element {

  final override def recognizes(elem: Elem): Boolean = (elem.label == label)
}
