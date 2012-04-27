/*
 *  Copyright 2011 Leonid Dubinsky <dub@podval.org>.
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

import scala.xml.Node

import org.podval.judaica.common.Xml.oneChild


final class Names(val names: Seq[Name]) {

  def find(name: String): Option[Name] = names.find(_.name == name)


  def has(name: String): Boolean = find(name).isDefined


  def byLang(lang: String): Option[Name] = names.find(_.lang == lang)


  def default: Name = names(0)


  override def toString: String = "Names: " + names
}



object Names {
  
  def apply(node: Node): Names = {
    new Names(oneChild(node, "names").child.map(Name(_)))
  }
}
