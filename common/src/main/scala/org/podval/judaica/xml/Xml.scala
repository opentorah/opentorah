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

package org.podval.judaica.xml


import scala.xml.{Elem, Text, XML, Utility, PrettyPrinter}

import java.io.{FileWriter, PrintWriter, File}


object Xml {

  def isDiv(elem: Elem, divType: String): Boolean = (elem.label == "div") && (getAttribute(elem, "type") == divType)


  def getAttribute(name: String)(elem: Elem): String = (elem \ ("@" + name)).text


  def getAttribute(elem: Elem, name: String): String = getAttribute(name)(elem)


  def getBooleanAttribute(elem: Elem, name: String): Boolean = getAttribute(elem, name) == "true"


  def booleanAttribute(value: Boolean) = if (value) Some(Text("true")) else None


  def oneChild(elem: Elem, name: String): Elem = {
    val children = elem \ name

    require(children.size > 0, "No child with name " + name)
    require(children.size == 1, "To many children with name " + name)

    val result = children(0)
    require(result.isInstanceOf[Elem])
    result.asInstanceOf[Elem]
  }


  def elems(elem: Elem): Seq[Elem] = elem.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])


  def check(elem: Elem, tag: String): Elem = {
    require(elem.label == tag, "Expected tag " + tag + " but got " + elem.label)
    elem
  }


  def print(xml: Elem, outFile: File) {
    val out = new PrintWriter(new FileWriter(outFile))
    val pretty = new PrettyPrinter(100, 4).format(xml)
    // TODO        out.println("<!DOCTYPE html>\n" + pretty)
    out.println(pretty)
    out.close()
  }
}
