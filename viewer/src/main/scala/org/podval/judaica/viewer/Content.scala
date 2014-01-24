/*
 *  Copyright 2014 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.xml.{Xml, XmlFile}
import Xml.Ops

import scala.xml.{Node, Elem, MetaData, Text, UnprefixedAttribute, TopScope}

import java.io.File


trait Content



// TODO Div with <head> and a collection of prefix divs
final case class DivContent(sort: String, n: Option[String], attributes: MetaData, head: Option[String], children: Seq[Content]) extends Content



final case class AppContent(readings: Map[String, Seq[Content]]) extends Content



final case class TextContent(text: String) extends Content



final case class ElemContent(elem: Elem) extends Content



object Content {

  private def fromXmlSeq(xmls: Seq[Node]): Seq[Content] = xmls.map(fromXml).flatten


  private def toXmlSeq(contents: Seq[Content]) = contents.map(toXml(_))


  def fromXml(xml: Node): Option[Content] = xml match {
    case xml: Text => Some(TextContent(xml.text))
    case xml: Elem => Some(
      xml.label match {
        case "div" => divFromXml(xml)
        case "app" => appFromXml(xml)
        case _ =>  println(s"Unrecognized Element ${xml.label}"); ElemContent(xml)
      })
    case _ => None
  }


  def toXml(content: Content): Node = content match {
    case text: TextContent => Text(text.text)
    case div: DivContent => divToXml(div)
    case app: AppContent => appToXml(app)
    case elem: ElemContent => elem.elem
  }


  private def divFromXml(xml: Elem): DivContent = {
    val sortOption = xml.attributeOption("type")
    if (sortOption.isEmpty) throw new ViewerException(s"No type for a div")

    val sort = sortOption.get
    val n: Option[String] = xml.attributeOption("n")
    val attributes = xml.attributes.filter(key => (key != "type") && (key != "n"))

    val hasHead = !xml.child.isEmpty && xml.child.head.isInstanceOf[Elem] && xml.child.head.label == "head"

    val head: Option[String] = if (!hasHead) None else Some(xml.child.head.text)

    val children: Seq[Content] = fromXmlSeq(if (!hasHead) xml.child else xml.child.tail)

    DivContent(sort, n, attributes, head, children)
  }


  private def divToXml(div: DivContent): Node = {
    val headElemOption = div.head.map(head => <head>{head}</head>)
    val childrenNodes = headElemOption.toSeq ++ toXmlSeq(div.children)

    Elem(
      null,
      "div",
      prependAttribute("type", div.sort, prependAttribute("n", div.n, div.attributes)),
      TopScope,
      true,
      childrenNodes: _*)
  }


  private def appFromXml(xml: Elem): AppContent = {
    val readings: Seq[Elem] = xml.child.map(_.asInstanceOf[Elem])
    readings.foreach(_.check("rdg"))
    AppContent(readings.map(reading => reading.getAttribute("type") -> fromXmlSeq(reading.child)).toMap)
  }


  private def appToXml(app: AppContent): Node =
    <app>{for ((sort, reading) <- app.readings) yield <rdg type={sort}>{toXmlSeq(reading)}</rdg>}</app>


  def prependAttribute(name: String, value: String, attributes: MetaData): MetaData =
    prependAttribute(name, Some(value), attributes)


  def prependAttribute(name: String, value: Option[String], attributes: MetaData): MetaData =
    value.fold(attributes)(v => new UnprefixedAttribute(name, Seq(Text(v)), attributes))


  def prependAttribute(name: String, value: Boolean, attributes: MetaData): MetaData =
    if (!value) attributes else new UnprefixedAttribute(name, Seq(Text("true")), attributes)
}



object Main {

  def main(args: Array[String]) {
    val file = Works.getWorkByName("Chumash").getEditionByName("Jerusalem").storage.storage("Genesis").asFile.file
    val xml = XmlFile.load(file)
    val content = Content.fromXml(xml).get
    val reXml = Content.toXml(content).asInstanceOf[Elem]
    val outFile = new File(file.getParentFile, "out.xml")
    println(s"Output File=$outFile")
    Xml.print(reXml, outFile)
  }
}
