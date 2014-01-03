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

import org.podval.judaica.xml.Xml

import scala.xml.Elem


trait Named {

  def names: Names
}



trait ByName[T <: Named] {

  def named: Seq[T]


  final def byName(name: String): Option[T] = named.find(_.names.has(name))
}



final class Names(name: Option[String], xml: Elem) {

  val names: Seq[Name] = {
    val rawNames: Seq[Name] = Xml.elems(xml, "names", "name").map(new Name(_))
    if (name.isDefined && !find(name.get, rawNames).isDefined) (new Name(name.get, "en", false) +: rawNames) else rawNames
  }


  def find(name: String): Option[Name] = find(name, names)


  private[this] def find(name: String, names: Seq[Name]): Option[Name] = names.find(_.name == name)


  def has(name: String): Boolean = find(name).isDefined


  def byLang(lang: String): Option[Name] = names.find(_.lang == lang)


  def default: Name = names(0)


  override def toString: String = "Names: " + names
}



object Names {

  def apply(name: String, xml: Elem): Names = new Names(Some(name), xml)


  def apply(xml: Elem): Names = new Names(None, xml)
}



final class Name(val name: String, val lang: String, val isTransliterated: Boolean) {

  def this(xml: Elem) = this(
    Xml.getAttribute(xml, "name"),
    Xml.getAttribute(xml, "lang"),
    Xml.getBooleanAttribute(xml, "isTransliterated")
  )


  override def toString: String =
    "Name: " + name + " (" + lang + (if (!isTransliterated) "" else ", " + isTransliterated) +  ")"
}
