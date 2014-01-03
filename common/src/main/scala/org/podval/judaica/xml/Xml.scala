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

package org.podval.judaica.xml


import scala.xml.{Node, Elem, Text, XML, PrettyPrinter}

import java.io.{FileWriter, PrintWriter, File}


object Xml {

  def isDiv(elem: Elem, divType: String): Boolean = (elem.label == "div") && (getAttribute(elem, "type") == divType)


  def getAttributeOption(elem: Elem, name: String): Option[String] = {
    val result: Seq[Node] = (elem \ ("@" + name))
    if (result.isEmpty) None else Some(result.text)
  }


  def getAttribute(elem: Elem, name: String): String = getAttributeOption(elem, name).get


  def getAttribute(name: String)(elem: Elem): String = getAttribute(elem, name)


  def getBooleanAttribute(elem: Elem, name: String): Boolean = {
    val value = getAttributeOption(elem, name)
    value.isDefined && (value.get == "true")
  }


  def booleanAttribute(value: Boolean) = if (value) Some(Text("true")) else None


  def oneChild(elem: Elem, name: String): Elem = oneOptionalChild(elem, name, true).get


  def oneOptionalChild(elem: Elem, name: String, required: Boolean = true): Option[Elem] = {
    val children = elem \ name

    require(children.size <= 1, "To many children with name " + name)

    if (required) require(children.size > 0, "No child with name " + name)

    if (children.isEmpty) None else Some(children(0).asInstanceOf[Elem])
  }


  def elems(elem: Elem, plural: String, singular: String, required: Boolean = true): Seq[Elem] = {
    val child = oneOptionalChild(elem, plural, required)
    if (child.isEmpty) Seq.empty else elems(child.get, singular)
  }


  def elems(elem: Elem): Seq[Elem] = elem.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])


  def elems(elem: Elem, name: String): Seq[Elem] = {
    val result = elems(elem)
    result.foreach(check(_, name))
    result
  }


  def check(elem: Elem, name: String): Elem = {
    require(elem.label == name, "Expected name " + name + " but got " + elem.label)
    elem
  }


  def print(xml: Elem, outFile: File) {
    val out = new PrintWriter(new FileWriter(outFile))
    val pretty = new PrettyPrinter(100, 4).format(xml)
    // TODO when outputting XML, include <xml> header?
    // TODO        out.println("<!DOCTYPE html>\n" + pretty)
    out.println(pretty)
    out.close()
  }
}
