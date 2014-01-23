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
import SelectorParser.ParsingContext

import scala.xml.Elem


object DivParser {

  private trait ParseableDominantDiv extends DominantDiv {

    protected final def parseDominantStructure(context: ParsingContext, xml: Elem): Structure = {
      val dominantXml = preParseStructures(structure.selector, xml).get(dominantSelector)
      if (dominantXml.isEmpty) throw new ViewerException(s"No dominant structure for $this")
      StructureParser.parseStructure(adjustContext(context), dominantSelector, this, dominantXml.get)
    }


    protected final def parseNonDominantStructures(context: ParsingContext, xml: Elem): Map[Selector, Structure] =
      parseStructures(
        adjustContext(context),
        this,
        preParseStructures(structure.selector, xml) - dominantSelector
      ) +
        (dominantSelector -> dominantStructure)


    private[this] def adjustContext(context: ParsingContext): ParsingContext =
      context.copy(dominantParentSelection = context.dominantParentSelection.selectDiv(this))
  }



  private trait ParseableNonDominantDiv extends NonDominantDiv {

    protected def parsePath(dominantParentSelection: StructureSelection, xml: Elem): Selection.Path = {
      val pathOption = xml.attributeOption("path")
      if (pathOption.isEmpty) throw new ViewerException(s"Div $id of the non-dominant structure must have a path")
      dominantParentSelection.parseDominantPath(pathOption.get)
    }
  }



  private abstract class ParseableDiv(context: ParsingContext, override val structure: Structure, xml: Elem) extends Div {

    private[this] val localSelectors: Seq[Selector] = SelectorParser.parseSelectors(context.knownSelectors, xml)


    override def selectors: Seq[Selector] = structure.selector.selectors ++ localSelectors
  }



  private abstract class ParseableNamedDiv(context: ParsingContext, structure: NamedStructure, xml: Elem)
    extends ParseableDiv(context, structure, xml) with NamedDiv
  {
    override final val names = Names(xml)
  }



  private final class DominantNamedDiv(context: ParsingContext, structure: NamedStructure, xml: Elem)
    extends ParseableNamedDiv(context, structure, xml) with ParseableDominantDiv
  {
    override val dominantStructure = parseDominantStructure(context, xml)


    override val structures = parseNonDominantStructures(context, xml)
  }



  private final class NonDominantNamedDiv(context: ParsingContext, structure: NamedStructure, xml: Elem)
    extends ParseableNamedDiv(context, structure, xml) with ParseableNonDominantDiv
  {
    override val path = parsePath(context.dominantParentSelection, xml)


    override val structures = parseStructures(context, this, xml)
  }



  def parseNamed(context: ParsingContext, structure: NamedStructure, xml: Elem): NamedDiv =
    if (context.isDominant)
      new DominantNamedDiv(context, structure, xml)
    else
      new NonDominantNamedDiv(context, structure, xml)



  private abstract class ParseableNumberedDiv(context: ParsingContext, structure: NumberedStructure, override val number: Int, xml: Elem)
    extends ParseableDiv(context, structure, xml) with NumberedDiv
  {
    xml.intAttributeOption("n").foreach { nvalue =>
      if (nvalue != number) throw new ViewerException(s"Div $number has attribute n set to $nvalue")
    }
  }



  private final class DominantNumberedDiv(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem)
    extends ParseableNumberedDiv(context, structure, number, xml) with ParseableDominantDiv
  {
    override val dominantStructure = parseDominantStructure(context, xml)


    override val structures = parseNonDominantStructures(context, xml)
  }



  private final class NonDominantNumberedDiv(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem)
    extends ParseableNumberedDiv(context, structure, number, xml) with ParseableNonDominantDiv
  {
    override val path = parsePath(context.dominantParentSelection, xml)


    override val structures = parseStructures(context, this, xml)
  }


  def parseNumbered(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem): NumberedDiv =
    if (context.isDominant)
      new DominantNumberedDiv(context, structure, number, xml)
    else
      new NonDominantNumberedDiv(context, structure, number, xml)



  private type Xmls = Map[Selector, Elem]


  def parseStructures(context: SelectorParser.ParsingContext, selectors: Selectors, xml: Elem): Map[Selector, Structure] =
    parseStructures(context, selectors, preParseStructures(selectors, xml))


  private def preParseStructures(selectors: Selectors, xmls: Elem): Xmls =
    xmls.elemsFilter("structure").map(xml => selectors.getSelectorByName(xml.getAttribute("selector")) -> xml).toMap


  // TODO verify that all structures requested by the selectors are present; some allowed structures need to be calculated...
  // TODO make sure that they are retrievable, too - for instance, week/chapter!
  ///    selectors.foreach(selector => Exists(structures, selector.defaultName, "structures"))
  private def parseStructures(context: SelectorParser.ParsingContext, selectors: Selectors, xmls: Xmls): Map[Selector, Structure] =
    for ((selector, xml) <- xmls) yield selector -> StructureParser.parseStructure(context, selector, selectors, xml)
}
