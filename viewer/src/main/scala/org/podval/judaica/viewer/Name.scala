/*
 *  Copyright 2011-2018 Leonid Dubinsky <dub@podval.org>.
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

trait Named {
  def names: Names

  final def defaultName: String = names.default.name
}


final class Name(val name: String, val lang: String, val isTransliterated: Boolean) {
  override def toString: String =
    "Name: " + name + " (" + lang + (if (!isTransliterated) "" else ", " + isTransliterated) +  ")"
}


final class Names(val names: Seq[Name]) {
  def find(name: String): Option[Name] = find(names, name)

  private[this] def find(names: Seq[Name], name: String): Option[Name] = names.find(_.name == name)

  def has(name: String): Boolean = find(name).isDefined

  def byLang(lang: Language): Option[Name] = names.find(_.lang == lang.name)

  def default: Name = names.head

  def isEmpty: Boolean = names.isEmpty

  override def toString: String = "Names: " + names
}


object Names {
  def find[T <: Named](nameds: Traversable[T], name: String): Option[T] = nameds.find(_.names.has(name))

  def doFind[T <: Named](nameds: Traversable[T], name: String, what: String): T = Exists(find(nameds, name), name, what)
}
