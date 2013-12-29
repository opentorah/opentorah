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

import scala.xml.Elem


abstract class ElementDisplayer {

  def recognizes(elem: Elem): Boolean


  def display(elem: Elem, displayers: Set[ElementDisplayer]): Seq[Elem]
}


object ElementDisplayer {

  def find(elem: Elem, displayers: Set[ElementDisplayer]): ElementDisplayer = {
    val result = displayers.find(_.recognizes(elem))
    if (result.isEmpty) throw new NoSuchElementException("No displayer for " + elem)
    result.get
  }
}