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
import Selector.ParsingContext

import scala.xml.Elem


abstract class Div(context: ParsingContext, val structure: Structure, xml: Elem) extends Structures {

  private[this] val localSelectors: Seq[Selector] = Selector.parse(context.knownSelectors, xml)


  override def selectors: Seq[Selector] = structure.selector.selectors ++ localSelectors


  def id: String
}



trait DominantDiv { self : Div =>

  val dominantStructure: Structure


  final def parseDominantStructure(context: ParsingContext, xml: Elem): Structure = {
    val dominantXml = structure.selector.preParseStructures(xml).get(dominantSelector)
    if (dominantXml.isEmpty) throw new ViewerException(s"No dominant structure for $this")
    dominantSelector.parseStructure(context, this, dominantXml.get)
  }


  protected final def parseNonDominantStructures(context: ParsingContext, xml: Elem): Map[Selector, Structure] =
    parseStructures(
      adjustContext(context),
      structure.selector.preParseStructures(xml) - dominantSelector
    ) +
      (dominantSelector -> dominantStructure)


  private[this] def adjustContext(context: ParsingContext): ParsingContext =
    context.copy(dominantParentSelection = context.dominantParentSelection.selectDiv(this))
}



trait NonDominantDiv { self: Div =>

  val path: Selection.Path


  protected def parsePath(dominantParentSelection: StructureSelection, xml: Elem): Selection.Path = {
    val pathOption = xml.attributeOption("path")
    if (pathOption.isEmpty) throw new ViewerException(s"Div $id of the non-dominant structure must have a path")
    dominantParentSelection.parseDominantPath(pathOption.get)
  }
}



abstract class NamedDiv(context: ParsingContext, structure: NamedStructure, xml: Elem)
  extends Div(context, structure, xml) with Named
{
  override final val names = Names(xml)


  override final def id: String = defaultName
}



final class DominantNamedDiv(context: ParsingContext, structure: NamedStructure, xml: Elem)
  extends NamedDiv(context, structure, xml) with DominantDiv
{
  override val dominantStructure = parseDominantStructure(context, xml)


  override val structures = parseNonDominantStructures(context, xml)
}



final class NonDominantNamedDiv(context: ParsingContext, structure: NamedStructure, xml: Elem)
  extends NamedDiv(context, structure, xml) with NonDominantDiv
{
  override val path = parsePath(context.dominantParentSelection, xml)


  override val structures = parseStructures(context, xml)
}



object NamedDiv {

  def apply(context: ParsingContext, structure: NamedStructure, xml: Elem): NamedDiv = {
    if (context.isDominant)
      new DominantNamedDiv(context, structure, xml)
    else
      new NonDominantNamedDiv(context, structure, xml)
  }
}



abstract class NumberedDiv(context: ParsingContext, structure: NumberedStructure, val number: Int, xml: Elem)
  extends Div(context, structure, xml)
{
  checkNumber

  private[this] def checkNumber {
    xml.intAttributeOption("n").foreach { nvalue =>
      if (nvalue != number) throw new ViewerException(s"Div $number has attribute n set to $nvalue")
    }
  }

  final override def id: String = number.toString
}



final class DominantNumberedDiv(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem)
  extends NumberedDiv(context, structure, number, xml) with DominantDiv
{
  override val dominantStructure = parseDominantStructure(context, xml)


  override val structures = parseNonDominantStructures(context, xml)
}



final class NonDominantNumberedDiv(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem)
  extends NumberedDiv(context, structure, number, xml) with NonDominantDiv
{
  override val path = parsePath(context.dominantParentSelection, xml)


  override val structures = parseStructures(context, xml)
}



object NumberedDiv {

  def apply(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem): NumberedDiv = {
    if (context.isDominant)
      new DominantNumberedDiv(context, structure, number, xml)
    else
      new NonDominantNumberedDiv(context, structure, number, xml)
  }
}
