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


import scala.xml.{Node, Elem, Text, PrettyPrinter}

import java.io.{FileWriter, PrintWriter, File}


object Xml {

  implicit class XmlOps(elem: Elem) {

    def elems(plural: String, singular: String, required: Boolean = true): Seq[Elem] = {
      val child = oneOptionalChild(plural, required)
      if (child.isEmpty) Seq.empty else child.get.elems(singular)
    }


    def elems(name: String): Seq[Elem] = {
      val result = elem.elems
      result.foreach(_.check(name))
      result
    }


    def elems: Seq[Elem] = elem.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])


    def getAttribute(name: String): String = attributeOption(name).get


    def booleanAttribute(name: String): Boolean = {
      val value = attributeOption(name)
      value.isDefined && (value.get == "true")
    }


    def attributeOption(name: String): Option[String] = {
      val result: Seq[Node] = (elem \ ("@" + name))
      if (result.isEmpty) None else Some(result.text)
    }


    def oneChild(name: String): Elem = oneOptionalChild(name, true).get


    private[this] def oneOptionalChild(name: String, required: Boolean = true): Option[Elem] = {
      val children = elem \ name

      require(children.size <= 1, "To many children with name " + name)

      if (required) require(children.size > 0, "No child with name " + name)

      if (children.isEmpty) None else Some(children(0).asInstanceOf[Elem])
    }


    def check(name: String): Elem = {
      require(elem.label == name, "Expected name " + name + " but got " + elem.label)
      elem
    }


    def isDiv(divType: String): Boolean = (elem.label == "div") && (elem.attribute("type") == divType)


    def print(outFile: File) {
      val out = new PrintWriter(new FileWriter(outFile))
      val pretty = new PrettyPrinter(100, 4).format(elem)
      // TODO when outputting XML, include <xml> header?
      // TODO        out.println("<!DOCTYPE html>\n" + pretty)
      out.println(pretty)
      out.close()
    }
  }




  def booleanAttribute(value: Boolean) = if (value) Some(Text("true")) else None
}
