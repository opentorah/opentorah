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

package org.podval.judaica.common


import scala.xml.{Node, Elem, Text, XML, Utility, PrettyPrinter}

import java.io.{FileWriter, PrintWriter, File}
import scala.Some


object Xml {

  def isDiv(node: Node): Boolean = node.isInstanceOf[Elem] && node.asInstanceOf[Elem].label == "div"


  def isDiv(node: Node, divType: String): Boolean = isDiv(node) && (getType(node) == divType)


  def getType(node: Node) = getAttribute(node, "type")


  def getAttribute(name: String)(node: Node): String = (node \ ("@" + name)).text


  def getAttribute(node: Node, name: String): String = getAttribute(name)(node)


  def getBooleanAttribute(node: Node, name: String): Boolean = getAttribute(node, name) == "true"


  def booleanAttribute(value: Boolean) = if (value) Some(Text("true")) else None


  def oneChild(node: Node, name: String): Node = {
    val children = node \ name

    if (children.size == 0) {
      throw new IllegalArgumentException("No child with name " + name)
    }

    if (children.size > 1) {
      throw new IllegalArgumentException("To many children with name " + name)
    }

    children(0)
  }


  def loadResource(clazz: Class[_], name: String, tag: String): Elem =
    open(XML.load(clazz.getResourceAsStream(name + ".xml")), tag)


  def loadFile(file: File, tag: String): Elem = open(XML.loadFile(file), tag)


  def loadFile(file: File): Elem = XML.loadFile(file)


  private def open(what: Node, tag: String): Elem =
    check(Utility.trimProper(what)(0).asInstanceOf[Elem], tag)


  def check(elem: Elem, tag: String): Elem =
    if (elem.label == tag) elem
    else throw new IllegalArgumentException("Expected tag " + tag + " but got " + elem.label)


  def wrapInHtml(stylesheet: String, what: Node) = {
    <html>
      <head>
        <link rel="stylesheet" type="text/css" href={stylesheet + ".css"}/>
      </head>
      <body class="hebrew">
        {what}
      </body>
    </html>
  }


  def print(xml: Node, outFile: File) {
    val out = new PrintWriter(new FileWriter(outFile))
    val pretty = new PrettyPrinter(100, 4).format(xml)
    // TODO        out.println("<!DOCTYPE html>\n" + pretty)
    out.println(pretty)
    out.close()
  }
}
