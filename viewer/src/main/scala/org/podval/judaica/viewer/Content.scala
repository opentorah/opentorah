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



final case class DivContent(
  prefix: Seq[Content],
  sort: String,
  n: Option[String],
  attributes: MetaData,
  head: Option[String],
  children: Seq[Content]
) extends Content



final case class AppContent(readings: Map[String, Seq[Content]]) extends Content



final case class TextContent(text: String) extends Content



final case class ElemContent(elem: Elem) extends Content



object DivContent {

  def fromXml(xml: Elem): DivContent = {
    val sortOption = xml.attributeOption("type")
    if (sortOption.isEmpty) throw new ViewerException(s"No type for a div")

    val sort = sortOption.get
    val n: Option[String] = xml.attributeOption("n")
    val attributes = xml.attributes.filter(key => (key != "type") && (key != "n"))

    val hasHead = !xml.child.isEmpty && xml.child.head.isInstanceOf[Elem] && xml.child.head.label == "head"

    val head: Option[String] = if (!hasHead) None else Some(xml.child.head.text)

    val children: Seq[Content] = Content.fromXmlSeq(if (!hasHead) xml.child else xml.child.tail)

    DivContent(Seq.empty, sort, n, attributes, head, children)
  }


  def toXml(div: DivContent): Seq[Node] = {
    val prefixNodes = Content.toXmlSeq(div.prefix)

    val headElemOption = div.head.map(head => <head>{head}</head>)

    val childrenNodes = headElemOption.toSeq ++ Content.toXmlSeq(div.children)

    prefixNodes :+
      Elem(
        null,
        "div",
        prependAttribute("type", div.sort, prependAttribute("n", div.n, div.attributes)),
        TopScope,
        true,
        childrenNodes: _*)
  }


  def prependAttribute(name: String, value: String, attributes: MetaData): MetaData =
    prependAttribute(name, Some(value), attributes)


  def prependAttribute(name: String, value: Option[String], attributes: MetaData): MetaData =
    value.fold(attributes)(v => new UnprefixedAttribute(name, Seq(Text(v)), attributes))


  def prependAttribute(name: String, value: Boolean, attributes: MetaData): MetaData =
    if (!value) attributes else new UnprefixedAttribute(name, Seq(Text("true")), attributes)


  def select(file: File, path: Div.Path, format: Selector.Format): Content = select(fromXml(XmlFile.load(file)), path, format)


  def select(content: DivContent, path: Div.Path, format: Selector.Format): Content =
    if (path.isEmpty) reformat(content, format) else {
      val contentStructure = guessStructure(content)
      val selectionStructure = path.head.structure.defaultName
      if (selectionStructure != contentStructure) throw new ViewerException(s"Restructuring from $contentStructure to $selectionStructure isn't yet supported")
      val groupped: DivContent = group(content, selectionStructure)
      val sub: DivContent = select(groupped, path.head)
      val subSelect = select(sub, path.tail, format)
      groupped.copy(children = Seq(subSelect))
    }


  def guessStructure(content: DivContent): String = {
    val nonEmptyDiv = content.children.find(c => c.isInstanceOf[DivContent] && !c.asInstanceOf[DivContent].children.isEmpty)
    if (nonEmptyDiv.isEmpty) throw new ViewerException(s"Can't guess structure")
    nonEmptyDiv.get.asInstanceOf[DivContent].sort
  }


  def group(content: DivContent, sort: String): DivContent = content.copy(children = group(content.children, sort))


  def group(children: Seq[Content], sort: String): Seq[Content] = if (children.isEmpty) Seq.empty else {
    val (prefix, rest) = children.span(c => !(c.isInstanceOf[DivContent] && (c.asInstanceOf[DivContent].sort == sort)))
    if (rest.isEmpty) {
      // Enclosing Div's trailer :)
      prefix
    } else {
      val div: DivContent = rest.head.asInstanceOf[DivContent]
      val grouppedDiv = div.copy(prefix = prefix)
      grouppedDiv +: group(rest.tail, sort)
    }
  }


  def select(content: DivContent, div: Div): DivContent = {
    val result = content.children.find(c => c.isInstanceOf[DivContent] && (c.asInstanceOf[DivContent].n == Some(div.id)))
    if (result.isEmpty) throw new ViewerException(s"Child not found")
    result.get.asInstanceOf[DivContent]
  }


  def reformat(content: DivContent, format: Selector.Format): DivContent = content // TODO implement
}



object Content {

  def fromXmlSeq(xmls: Seq[Node]): Seq[Content] = xmls.map(fromXml).flatten


  def toXmlSeq(contents: Seq[Content]): Seq[Node] = contents.flatMap(toXml(_))


  def fromXml(xml: Node): Option[Content] = xml match {
    case xml: Text => Some(TextContent(xml.text))
    case xml: Elem => Some(
      xml.label match {
        case "div" => DivContent.fromXml(xml)
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
    case app: AppContent => Seq(appToXml(app))
    case elem: ElemContent => Seq(elem.elem)
  }


  private def appFromXml(xml: Elem): AppContent = {
    val readings: Seq[Elem] = xml.child.map(_.asInstanceOf[Elem])
    readings.foreach(_.check("rdg"))
    AppContent(readings.map(reading => reading.getAttribute("type") -> fromXmlSeq(reading.child)).toMap)
  }


  private def appToXml(app: AppContent): Node =
    <app>{for ((sort, reading) <- app.readings) yield <rdg type={sort}>{toXmlSeq(reading)}</rdg>}</app>


  def textElem(name: String, text: String): ElemContent = {
    ElemContent(Elem(
      null,
      name,
      Node.NoAttributes,
      TopScope,
      true,
      Text(text)
    ))
  }
}



object Main {

  def main(args: Array[String]) {
    val file = Works.getWorkByName("Chumash").getEditionByName("Jerusalem").storage.storage("Genesis").asFile.file
    val xml = XmlFile.load(file)
    val content = DivContent.fromXml(xml)
    val reXml = Content.toXml(content).asInstanceOf[Elem]
    val outFile = new File(file.getParentFile, "out.xml")
    println(s"Output File=$outFile")
    Xml.print(reXml, outFile)
  }
}
