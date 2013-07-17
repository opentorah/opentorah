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
 */

package org.podval.judaica.viewer

import scala.xml.Elem

import org.podval.judaica.xml.Xml
import Xml.getAttribute


final class Name(val name: String, val lang: String, val isTransliterated: Boolean) {
  
  override def toString: String =
    "Name: " + name + " (" + lang + (if (!isTransliterated) "" else ", " + isTransliterated) +  ")"
}


object Name {
  
  def apply(node: Elem): Name = {
    Xml.check(node.asInstanceOf[Elem], "name")
    
    new Name(
      getAttribute(node, "name"),
      getAttribute(node, "lang"),
      getAttribute(node, "isTransliterated") == "true"
    )
  }
}
