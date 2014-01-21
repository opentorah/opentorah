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

import java.io.{File, OutputStream, Writer, FileWriter, OutputStreamWriter, PrintWriter}

// TODO eliminate this dependency
import org.podval.judaica.viewer.ViewerException
import scala.Some


object Xml {

  implicit class Ops(elem: Elem) {

    def elems(plural: String, singular: String, required: Boolean = true): Seq[Elem] =
      oneOptionalChild(plural, required).map(_.elems(singular)).getOrElse(Seq.empty)


    def elems(name: String): Seq[Elem] = {
      val result = elem.elems
      result.foreach(_.check(name))
      result
    }


    def elemsFilter(name: String): Seq[Elem] = elem.elems.filter(_.label == name)


    def elems: Seq[Elem] = elem.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])


    def getAttribute(name: String): String = attributeOption(name).getOrElse(throw new ViewerException(s"No attribute $name"))


    def attributeOption(name: String): Option[String] = {
      val result: Seq[Node] = (elem \ ("@" + name))
      if (result.isEmpty) None else Some(result.text)
    }


    def intAttributeOption(name: String): Option[Int] = attributeOption(name).map { value =>
      try { value.toInt } catch { case e: NumberFormatException => throw new ViewerException(s"$value is not a number", e)}
    }


    def intAttribute(name: String): Int = intAttributeOption(name).getOrElse(throw new ViewerException(s"No attribute $name"))


    def booleanAttribute(name: String): Boolean = {
      val value = attributeOption(name)
      value.isDefined && (value.get == "true")
    }


    def oneChild(name: String): Elem = oneOptionalChild(name, true).get


    private[this] def oneOptionalChild(name: String, required: Boolean = true): Option[Elem] = {
      val children = elem \ name

      if (children.size > 1) throw new ViewerException(s"To many children with name $name")
      if (required && children.isEmpty) throw new ViewerException(s"No child with name $name")

      if (children.isEmpty) None else Some(children(0).asInstanceOf[Elem])
    }


    def check(name: String): Elem = {
      if (elem.label != name) throw new ViewerException(s"Expected name $name but got $elem.label")
      elem
    }


    def isDiv(divType: String): Boolean = (elem.label == "div") && (elem.attribute("type") == divType)


    def print(outStream: OutputStream): Unit = print(new OutputStreamWriter(outStream))
    def print(outFile: File): Unit = print(new FileWriter(outFile))

    def print(writer: Writer) {
      val out = new PrintWriter(writer)
      val pretty = new PrettyPrinter(100, 4).format(elem)
      // TODO when outputting XML, include <xml> header?
      // TODO        out.println("<!DOCTYPE html>\n" + pretty)
      out.println(pretty)
      out.close()
    }
  }



  def booleanAttribute(value: Boolean) = if (value) Some(Text("true")) else None
}
