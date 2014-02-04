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

import org.podval.judaica.xml.Xml.Ops
import org.podval.judaica.xml.XmlFile
import scala.xml.{Elem, Node, MetaData, UnprefixedAttribute, TopScope, Text}
import java.io.File


trait DivContent extends Content {

  def prefix: Seq[Content]


  def sort: String


  def n: Option[String]


  def attributes: MetaData


  def head: Option[String]


  def children: Seq[Content]
}



final case class ParsedDivContent(
  override val prefix: Seq[Content],
  override val sort: String,
  override val n: Option[String],
  override val attributes: MetaData,
  override val head: Option[String],
  override val children: Seq[Content]
) extends DivContent



final case class BoundDivContent(
  val div: Div,
  val lang: Language,
  override val prefix: Seq[Content],
  override val attributes: MetaData,
  override val children: Seq[Content]) extends DivContent
{
  override def sort: String = div.structure.defaultName


  override val n: Option[String] = Some(div.id)


  override val head: Option[String] = Some(div.name(lang))
}



object DivContent {

  def apply(sort: String, n: Option[String], attributes: MetaData, head: Option[String], children: Seq[Content]): DivContent =
    new ParsedDivContent(Seq.empty, sort, n, attributes, head, children)



  def replaceChildren(content: DivContent, children: Seq[Content]): DivContent =
    new ParsedDivContent(content.prefix, content.sort, content.n, content.attributes, content.head, children)


  def replacePrefix(content: DivContent, prefix: Seq[Content]): DivContent =
    new ParsedDivContent(prefix, content.sort, content.n, content.attributes, content.head, content.children)


  def fromXml(xml: Elem): DivContent = {
    val sortOption = xml.attributeOption("type")
    if (sortOption.isEmpty) throw new ViewerException(s"No type for a div")

    val sort = sortOption.get
    val n: Option[String] = xml.attributeOption("n")
    val attributes = xml.attributes.filter(key => (key != "type") && (key != "n"))

    val hasHead = !xml.child.isEmpty && xml.child.head.isInstanceOf[Elem] && xml.child.head.label == "head"

    val head: Option[String] = if (!hasHead) None else Some(xml.child.head.text)

    val children: Seq[Content] = Content.fromXmlSeq(if (!hasHead) xml.child else xml.child.tail)

    DivContent(sort, n, attributes, head, children)
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
      replaceChildren(groupped, Seq(subSelect))
    }


  def bind(content: DivContent, parentDiv: Div, lang: Language): DivContent = {
    val selectorOption = parentDiv.selectorByName(content.sort)
    if (selectorOption.isEmpty) content else {
      val structure = parentDiv.getStructure(selectorOption.get)
      val divName = content.n.get
      val divOption = structure.divById(divName)
      if (divOption.isEmpty) throw new ViewerException(s"No Div $divName in $structure")
      bindWithThis(content, divOption.get, lang)
    }
  }


  def bindWithThis(content: DivContent, div: Div, lang: Language): DivContent = {
    def bindSeq(contents: Seq[Content], parentDiv: Div): Seq[Content] = contents.map { c =>
      if (c.isInstanceOf[DivContent]) bind(c.asInstanceOf[DivContent], parentDiv, lang) else c
    }

    new BoundDivContent(
      div,
      lang,
      bindSeq(content.prefix, div),
      content.attributes,
      bindSeq(content.children, div)
    )
  }


  def guessStructure(content: DivContent): String = {
    val nonEmptyDiv = content.children.find(c => c.isInstanceOf[DivContent] && !c.asInstanceOf[DivContent].children.isEmpty)
    if (nonEmptyDiv.isEmpty) throw new ViewerException(s"Can't guess structure")
    nonEmptyDiv.get.asInstanceOf[DivContent].sort
  }


  def group(content: DivContent, sort: String): DivContent = replaceChildren(content, group(content.children, sort))


  def group(children: Seq[Content], sort: String): Seq[Content] = if (children.isEmpty) Seq.empty else {
    val (prefix, rest) = children.span(c => !(c.isInstanceOf[DivContent] && (c.asInstanceOf[DivContent].sort == sort)))
    if (rest.isEmpty) {
      // Enclosing Div's trailer :)
      prefix
    } else {
      val div: DivContent = rest.head.asInstanceOf[DivContent]
      val grouppedDiv = replacePrefix(div, prefix)
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
