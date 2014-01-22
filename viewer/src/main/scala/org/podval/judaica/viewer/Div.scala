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


// TODO I am generating terminal Divs; should define equals method on them so that things work :)
// TODO introduce dominant/non-dominant Structure, so that the Work is covered too...
trait Div extends Structures with Ordered[Div] {

  def isDominant: Boolean


  def asDominant: DominantDiv


  def structure: Structure


  def id: String


  final override def compare(that: Div): Int = structure.compare(this, that)
}



trait NamedDiv extends Div with Named {

  final override def id: String = defaultName
}



trait NumberedDiv extends Div {

  val number: Int


  final override def id: String = number.toString
}



trait DominantDiv extends Div {

  final override def isDominant: Boolean = true


  final override def asDominant: DominantDiv = this


  def dominantStructure: Structure
}



trait NonDominantDiv extends Div {

  final override def isDominant: Boolean = false


  final override def asDominant: DominantDiv = throw new ViewerException(s"$this is not a dominant Div")


  val path: Selection.Path
}



final class GeneratedTerminalDominantNumberedDiv(override val structure: Structure, override val number: Int)
  extends Div with DominantDiv with NumberedDiv
{
  final override def selectors: Seq[Selector] = Seq.empty


  final override def structures: Map[Selector, Structure] = Map.empty


  final override def dominantStructure: Structure = throw new UnsupportedOperationException
}



object DivParser {

  import org.podval.judaica.xml.Xml.Ops
  import Selector.ParsingContext

  import scala.xml.Elem


  private trait ParseableDominantDiv extends DominantDiv {

    protected final def parseDominantStructure(context: ParsingContext, xml: Elem): Structure = {
      val dominantXml = structure.selector.preParseStructures(xml).get(dominantSelector)
      if (dominantXml.isEmpty) throw new ViewerException(s"No dominant structure for $this")
      StructureParser.parseStructure(adjustContext(context), dominantSelector, this, dominantXml.get)
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



  private trait ParseableNonDominantDiv extends NonDominantDiv {

    protected def parsePath(dominantParentSelection: StructureSelection, xml: Elem): Selection.Path = {
      val pathOption = xml.attributeOption("path")
      if (pathOption.isEmpty) throw new ViewerException(s"Div $id of the non-dominant structure must have a path")
      dominantParentSelection.parseDominantPath(pathOption.get)
    }
  }



  private abstract class ParseableDiv(context: ParsingContext, override val structure: Structure, xml: Elem) extends Div {

    private[this] val localSelectors: Seq[Selector] = Selector.parse(context.knownSelectors, xml)


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


    override val structures = parseStructures(context, xml)
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


    override val structures = parseStructures(context, xml)
  }


  def parseNumbered(context: ParsingContext, structure: NumberedStructure, number: Int, xml: Elem): NumberedDiv =
    if (context.isDominant)
      new DominantNumberedDiv(context, structure, number, xml)
    else
      new NonDominantNumberedDiv(context, structure, number, xml)
}
