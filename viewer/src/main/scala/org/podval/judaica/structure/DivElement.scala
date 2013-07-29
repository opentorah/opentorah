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
import org.podval.judaica.html.Html

import scala.xml.Elem


class DivElement(type_ : String, nameClassSuffix: Option[String] = Some("name"))(children: Element*) extends Element {

  final override def recognizes(elem: Elem): Boolean = (elem.label == "div") && (Xml.getAttribute(elem, "type") == type_)


  override def display(elem: Elem): Seq[Elem] =
    if (isEmpty) prefix(elem) ++ suffix(elem) else Seq(displayDiv(elem))


  final def displayDiv(elem: Elem): Elem = {
    val result = prefix(elem) ++ Xml.elems(elem).flatMap(e => findChild(e).display(e)) ++ suffix(elem)
    <div class={type_}>{result}</div>
  }


  protected def prefix(elem: Elem): Seq[Elem] = nameSpan(displayName(Xml.getAttribute(elem, "n"))).toSeq


  protected final def nameSpan(name: Option[String]): Option[Elem] = name.map(n => Html.span(nameClass, n))


  private val nameClass = if (nameClassSuffix.isDefined) type_ + "-" + nameClassSuffix.get else type_


  private[this] def isEmpty: Boolean = children.isEmpty


  private[this] def findChild(elem: Elem): Element = {
    val result = children.find(_.recognizes(elem))
    if (result.isEmpty) throw new NoSuchElementException("Children of " + this + " do not recognize " + elem)
    result.get
  }


  protected def displayName(n: String): Option[String] = Some(n)


  protected def suffix(elem: Elem): Seq[Elem] = Seq.empty


  override def toString: String = "DivElement " + type_
}
