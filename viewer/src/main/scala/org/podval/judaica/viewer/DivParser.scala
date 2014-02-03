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
import StructureParser.ParsingContext

import scala.xml.Elem


object DivParser {

  private abstract class ParsedDiv(context: ParsingContext, override val structure: Structure, xml: Elem) extends Div {

    private[this] val localSelectors: Seq[Selector] = SelectorParser.parseSelectors(context.knownSelectors, xml)


    override def selectors: Seq[Selector] = structure.selector.selectors ++ localSelectors
  }



  private abstract class ParsedNamedDiv(context: ParsingContext, structure: NamedStructure, xml: Elem)
    extends ParsedDiv(context, structure, xml) with NamedDiv
  {
    override final val names = Names(xml)
  }



  private final class DominantNamedDiv(context: ParsingContext, structure: NamedStructure, xml: Elem)
    extends ParsedNamedDiv(context, structure, xml) with DominantDiv
  {
    override val dominantStructure = parseDominantStructure(context, this, xml)


    override val structures = parseNonDominantStructures(context, this, xml)
  }



  private final class NonDominantNamedDiv(context: ParsingContext, structure: NamedStructure, xml: Elem)
    extends ParsedNamedDiv(context, structure, xml) with NonDominantDiv
  {
    override val dominantAnchor = parsePath(context.dominantParentSelection, xml)


    override val structures = parseStructures(context, this, xml)
  }



  def parseNamed(context: ParsingContext, structure: NamedStructure, xml: Elem): NamedDiv =
    if (context.isDominant)
      new DominantNamedDiv(context, structure, xml)
    else
      new NonDominantNamedDiv(context, structure, xml)



  private abstract class ParsedNumberedDiv(context: ParsingContext, structure: NumberedStructure, override val number: Int, xml: Elem)
    extends ParsedDiv(context, structure, xml) with NumberedDiv
  {
    xml.intAttributeOption("n").foreach { nvalue =>
      if (nvalue != number) throw new ViewerException(s"Div $number has attribute n set to $nvalue")
    }
  }



  private final class DominantNumberedDiv(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem)
    extends ParsedNumberedDiv(context, structure, number, xml) with DominantDiv
  {
    override val dominantStructure = parseDominantStructure(context, this, xml)


    override val structures = parseNonDominantStructures(context, this, xml)
  }



  private final class NonDominantNumberedDiv(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem)
    extends ParsedNumberedDiv(context, structure, number, xml) with NonDominantDiv
  {
    override val dominantAnchor = parsePath(context.dominantParentSelection, xml)


    override val structures = parseStructures(context, this, xml)
  }


  def parseNumbered(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem): NumberedDiv =
    if (context.isDominant)
      new DominantNumberedDiv(context, structure, number, xml)
    else
      new NonDominantNumberedDiv(context, structure, number, xml)



  private type Xmls = Map[Selector, Elem]


  def parseDominantStructure(context: ParsingContext, div: DominantDiv, xml: Elem): NonRootStructure = {
    val dominantXml = preParseStructures(div.asInstanceOf[Selectors], xml).get(div.dominantSelector)
    if (dominantXml.isEmpty) throw new ViewerException(s"No dominant structure for $div")
    StructureParser.parseStructure(div, adjustContext(context, div), div.dominantSelector, dominantXml.get)
  }


  def parseNonDominantStructures(context: ParsingContext, div: DominantDiv, xml: Elem): Map[Selector, NonRootStructure] =
    parseStructures(
      adjustContext(context, div),
      div,
      preParseStructures(div.asInstanceOf[Selectors], xml) - div.dominantSelector
    ) +
      (div.dominantSelector -> div.dominantStructure)


  private def adjustContext(context: ParsingContext, div: Div): ParsingContext =
    context.copy(dominantParentSelection = context.dominantParentSelection.selectDiv(div))


  private def parseStructures(context: ParsingContext, div: Div, xml: Elem): Map[Selector, NonRootStructure] =
    parseStructures(context, div, preParseStructures(div, xml))


  private def preParseStructures(selectors: Selectors, xmls: Elem): Xmls =
    xmls.elemsFilter("structure").map(xml => selectors.getSelectorByName(xml.getAttribute("selector")) -> xml).toMap


  // TODO verify that all structures requested by the selectors are present; some allowed structures need to be calculated...
  // TODO make sure that they are retrievable, too - for instance, week/chapter!
  ///    selectors.foreach(selector => Exists(structures, selector.defaultName, "structures"))
  private def parseStructures(context: ParsingContext, div: Div, xmls: Xmls): Map[Selector, NonRootStructure] =
    for ((selector, xml) <- xmls) yield selector -> StructureParser.parseStructure(div, context, selector, xml)


  private def parsePath(dominantParentSelection: StructureSelection, xml: Elem): Div = {
    val pathOption = xml.attributeOption("path")
    if (pathOption.isEmpty) throw new ViewerException(s"Div of the non-dominant structure must have a path")
    dominantParentSelection.parseDominantPath(pathOption.get)
  }
}
