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

package org.podval.judaica.xml

import Xml.XmlOps

import scala.xml.Elem


object Div {

  def apply(type_ : String, n : String, contents: Seq[Elem]): Elem =
    <div type={type_} n={n}>{contents}</div>


  def unapply(elem: Elem): Option[(String, String, Seq[Elem])] =
    if (elem.label == "div") Some((
      elem.getAttribute("type"),
      elem.getAttribute("n"),
      elem.elems
    )) else None
}
