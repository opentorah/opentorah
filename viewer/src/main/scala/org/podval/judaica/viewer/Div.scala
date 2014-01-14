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
import java.io.File


abstract class Div(val structure: Structure, parsingFile: File, uncles: Seq[Selector], xml: Elem) extends Structures {

  // TODO parse the Path
/////  val path = xml.attributeOption("path")
  val localSelectors: Seq[Selector] = Selector.parseSelectors(uncles, xml)
  override val selectors: Seq[Selector] = structure.selector.selectors ++ localSelectors
  override val structures: Seq[Structure] = Structure.parseStructures(parsingFile, selectors, xml)
  // TODO complete the list of structures - even if some of them are empty!

  override def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)
  override def structureByName(name: String): Option[Structure] = Names.find(structures, name)

  def isNumbered: Boolean
  def asNumbered: NumberedDiv
  def asNamed: NamedDiv

  def id: String
}


final class NamedDiv(structure: NamedStructure, parsingFile: File, uncles: Seq[Selector], xml: Elem)
  extends Div(structure, parsingFile, uncles, xml) with Named
{
  override val names = Names(xml)

  override def isNumbered: Boolean = false
  override def asNumbered: NumberedDiv = throw new ClassCastException
  override def asNamed: NamedDiv = this

  override def id: String = defaultName
}



final class NumberedDiv(structure: NumberedStructure, parsingFile: File, uncles: Seq[Selector], val number: Int, xml: Elem)
  extends Div(structure, parsingFile, uncles, xml)
{
  checkNumber

  private[this] def checkNumber {
    val nOption = xml.attributeOption("n")
    if (nOption.isDefined) {
      val nvalue = xml.getIntAttribute("n")
      if (nvalue != number) throw new ViewerException(s"Div $number has attribute n set to $nvalue")
    }
  }

  override def isNumbered: Boolean = true
  override def asNumbered: NumberedDiv = this
  override def asNamed: NamedDiv = throw new ClassCastException

  override def id: String = number.toString
}
