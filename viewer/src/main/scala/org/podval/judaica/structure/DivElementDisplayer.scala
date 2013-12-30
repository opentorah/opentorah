/*
 *  Copyright 2013 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.xml.{Xml, Html}

import scala.xml.Elem


class DivElementDisplayer(type_ : String) extends ElementDisplayer {

  final override def recognizes(elem: Elem): Boolean = (elem.label == "div") && (Xml.getAttribute(elem, "type") == type_)


  override def display(elem: Elem, displayers: Set[ElementDisplayer]): Seq[Elem] = {
    // TODO redo with for comprehension
    val nOption = Xml.getAttributeOption("n")(elem)
    val nameOption: Option[Elem] = if (nOption.isEmpty) None else {
      val n = nOption.get
      val name = if (!isNumeric) displayName(n) else {
        val number = n.toInt
        if (isOrdinal) HebrewNumbers.ordinal(number) else HebrewNumbers.fromInt(number)
      }
      val nameClass: String = type_ + "-" + (if (!isNumeric) "name" else "number")
      Some(Html.span(nameClass, name))
    }

    val children: Seq[Elem] = Xml.elems(elem)

    val content: Seq[Elem] =
      if (!children.isEmpty) children.flatMap(e => ElementDisplayer.find(e, displayers).display(e, displayers))
      else displayContent(elem)

    val suffixOption: Option[Elem] = suffix(elem)

    val result: Seq[Elem] = (nameOption ++ content ++ suffixOption).toSeq

    if (result.isEmpty)
      throw new IllegalArgumentException("nothing to display!")

    if (!children.isEmpty) Seq(<div class={type_}>{result}</div>) else result
  }


  protected def isNumeric: Boolean = false


  protected def isOrdinal: Boolean = false


  protected def displayName(n: String): String = n


  protected def displayContent(elem: Elem): Seq[Elem] = Seq.empty // TODO Elem?


  protected def suffix(elem: Elem): Option[Elem] = None


  override def toString: String = "DivDisplayer " + type_
}
