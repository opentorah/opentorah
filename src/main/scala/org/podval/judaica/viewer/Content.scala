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

import Xml.Ops

import scala.xml.{Node, Elem, Text}



trait Content



final case class AppContent(readings: Map[String, Seq[Content]]) extends Content



final case class TextContent(text: String) extends Content



final case class ElemContent(elem: Elem) extends Content


final case class SpanContent(sort: String, text: String) extends Content



object Content {

  def fromXmlSeq(xmls: Seq[Node]): Seq[Content] = xmls.map(fromXml).flatten


  def toXmlSeq(contents: Seq[Content]): Seq[Node] = contents.flatMap(toXml(_))


  def fromXml(xml: Node): Option[Content] = xml match {
    case xml: Text => Some(TextContent(xml.text))
    case xml: Elem => Some(
      xml.label match {
        case "div" => DivContent.fromXml(xml)
        case "span" => spanFromXml(xml)
        case "app" => appFromXml(xml)
        case _ =>  println(s"Unrecognized Element ${xml.label}"); ElemContent(xml)
      })
    case _ => None
  }


  def toXmlNode(content: Content): Elem = {
    val result = toXml(content)
    if (result.size != 1) throw new ViewerException(s"Must be exactly one Node")
    result.head.asInstanceOf[Elem]
  }


  def toXml(content: Content): Seq[Node] = content match {
    case text: TextContent => Seq(Text(text.text))
    case div: DivContent => DivContent.toXml(div)
    case span: SpanContent => spanToXml(span)
    case app: AppContent => Seq(appToXml(app))
    case elem: ElemContent => Seq(elem.elem)
  }


  def spanFromXml(xml: Elem): SpanContent = {
    val sort = xml.getAttribute("type")
    val text = xml.text
    SpanContent(sort, text)
  }


  def spanToXml(span: SpanContent): Elem =
    <span type={span.sort}>{span.text}</span>


  private def appFromXml(xml: Elem): AppContent = {
    val readings: Seq[Elem] = xml.child.map(_.asInstanceOf[Elem])
    readings.foreach(_.check("rdg"))
    AppContent(readings.map(reading => reading.getAttribute("type") -> fromXmlSeq(reading.child)).toMap)
  }


  private def appToXml(app: AppContent): Node =
    <app>{for ((sort, reading) <- app.readings) yield <rdg type={sort}>{toXmlSeq(reading)}</rdg>}</app>
}
