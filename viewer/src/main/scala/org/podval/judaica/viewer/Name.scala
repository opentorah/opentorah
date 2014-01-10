/*
 *  Copyright 2011-2014 Leonid Dubinsky <dub@podval.org>.
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


trait Named {

  def names: Names
}



final class Name(val name: String, val lang: String, val isTransliterated: Boolean) {

  override def toString: String =
    "Name: " + name + " (" + lang + (if (!isTransliterated) "" else ", " + isTransliterated) +  ")"
}



object Name {

  def apply(xml: Elem) = new Name(
    xml.getAttribute("name"),
    xml.getAttribute("lang"),
    xml.booleanAttribute("isTransliterated")
  )
}



// TODO factor the parsing out
final class Names(name: Option[String], xml: Elem, canBeEmpty: Boolean) {

  val names: Seq[Name] = {
    val rawNames: Seq[Name] = xml.elemsFilter("name").map(Name(_))
    val isEmpty = rawNames.isEmpty
    require(canBeEmpty || !isEmpty, "No names found")
    // TODO can this adding of the name be eliminated?
    if (!isEmpty && name.isDefined && !find(rawNames, name.get).isDefined) (new Name(name.get, "en", false) +: rawNames) else rawNames
  }


  def find(name: String): Option[Name] = find(names, name)


  private[this] def find(names: Seq[Name], name: String): Option[Name] = names.find(_.name == name)


  def has(name: String): Boolean = find(name).isDefined


  def byLang(lang: String): Option[Name] = names.find(_.lang == lang)


  def default: Name = names(0)


  def isEmpty: Boolean = names.isEmpty


  override def toString: String = "Names: " + names
}



object Names {

//  def apply(name: String, xml: Elem, canBeEmpty: Boolean = false): Names = new Names(Some(name), xml, canBeEmpty)


  def apply(xml: Elem, canBeEmpty: Boolean = false): Names = new Names(None, xml, canBeEmpty)


  def find[T <: Named](nameds: Seq[T], name: String): Option[T] = nameds.find(_.names.has(name))
}
