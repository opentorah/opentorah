/*
 *  Copyright 2014-2018 Leonid Dubinsky <dub@podval.org>.
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

package org.podval.judaica.parsers

import java.io.File
import org.digitaljudaica.store.metadata.Xml.Ops
import ParseException.withMetadataFile
import org.podval.judaica.viewer.{Div, NamedDiv, NamedSelector, NamedStructure, NonRootStructure, NumberedDiv,
  NumberedSelector, NumberedStructure, Selector, Selectors, StructureSelection, ViewerException}
import scala.xml.Elem

object StructureParser {

  case class ParsingContext(
    isDominant: Boolean,
    dominantParentSelection: StructureSelection,
    parsingFile: File,
    knownSelectors: Set[Selector]
  )


  private abstract class ParsedStructure(
    final override val parentDiv: Div,
    protected final override val lengthOption: Option[Int]
  ) extends NonRootStructure



  private abstract class ParsedNamedStructure(div: Div, final override val selector: NamedSelector, lengthOption: Option[Int], xml: Elem)
    extends ParsedStructure(div, lengthOption) with NamedStructure
  {
    protected final def parseDivs(context: ParsingContext, xml: Elem): Seq[NamedDiv] =
      checkLength(selector, lengthOption, divXmls(xml).map(xml => DivParser.parseNamed(context, this, xml)))
  }



  private final class EagerNamedStructure(div: Div, context: ParsingContext, selector: NamedSelector, lengthOption: Option[Int], xml: Elem)
    extends ParsedNamedStructure(div, selector, lengthOption, xml)
  {
    override val divs: Seq[NamedDiv] = parseDivs(context, xml)
  }



  private final class LazyNamedStructure(div: Div, context: ParsingContext, selector: NamedSelector, lengthOption: Option[Int], xml: Elem)
    extends ParsedNamedStructure(div, selector, lengthOption, xml)
  {
    override def divs: Seq[NamedDiv] = divs_.get


    private[this] val divs_ = LazyLoad(withMetadataFile(context.parsingFile)(xml => parseDivs(context, open(xml))))
  }



  private abstract class ParsedNumberedStructure(div: Div, final override val selector: NumberedSelector, lengthOption: Option[Int], xml: Elem)
    extends ParsedStructure(div, lengthOption) with NumberedStructure
  {
    protected final def parseDivs(context: ParsingContext, xml: Elem): Seq[NumberedDiv] =
      checkLength(selector, lengthOption, divXmls(xml).zipWithIndex.map { case (xml, num) => DivParser.parseNumbered(context, this, num+1, xml) })
  }



  private final class EagerNumberedStructure(div: Div, context: ParsingContext, selector: NumberedSelector, lengthOption: Option[Int], xml: Elem)
    extends ParsedNumberedStructure(div, selector, lengthOption, xml)
  {
    // TODO check that the Paths for the non-dominant structure are properly ordered; same for Named
    override val divs: Seq[NumberedDiv] = parseDivs(context, xml)
  }



  private final class LazyNumberedStructure(div: Div, context: ParsingContext, selector: NumberedSelector, lengthOption: Option[Int], xml: Elem)
    extends ParsedNumberedStructure(div, selector, lengthOption, xml)
  {
    override def divs: Seq[NumberedDiv] = divs_.get


    private[this] val divs_ = LazyLoad(withMetadataFile(context.parsingFile)(xml => parseDivs(context, open(xml))))
  }


  def parseStructure(div: Div, context: ParsingContext, selector: Selector, xml: Elem): NonRootStructure = {
    val lengthOption: Option[Int] = xml.intAttributeOption("length")

    val nextContext = context.copy(
      isDominant = context.isDominant && div.isDominantSelector(selector),
      knownSelectors = cousins(selector, div))

    xml.attributeOption("file").fold {
      if (selector.isNumbered)
        new EagerNumberedStructure(div, context, selector.asNumbered, lengthOption, xml)
      else
        new EagerNamedStructure(div, context, selector.asNamed, lengthOption, xml)
    }{
      fileName: String =>
        import java.io.File

        val nextParsingFile: File = new File(context.parsingFile.getParentFile, fileName)
        val realNextContext = nextContext.copy(parsingFile = nextParsingFile)

        if (selector.isNumbered)
          new LazyNumberedStructure(div, realNextContext, selector.asNumbered, lengthOption, xml)
        else
          new LazyNamedStructure(div, realNextContext, selector.asNamed, lengthOption, xml)
    }
  }


  private def open(xml: Elem): Elem = {
    val result = xml.oneChild("structure")
    // TODO check that this is the correct structure :)
    result
  }


  private def divXmls(xml: Elem): Seq[Elem] = xml.elemsFilter("div")


  // TODO check that named or non-dominant structure is fully defined
  // TODO maybe structure for the terminal selectors shouldn't be *allowed*, not just allowed to be omitted...
  // TODO length should really be supplied for the terminal structure!
  private def checkLength[T <: Div](selector: Selector, lengthOption: Option[Int], divs: Seq[T]): Seq[T] = {
    if (!selector.isTerminal && lengthOption.isDefined && lengthOption.get != divs.length)
      throw new ViewerException(s"Wrong length: expected ${lengthOption.get} but got ${divs.length}")

    divs
  }


  private def cousins(selector: Selector, selectors: Selectors): Set[Selector] = {
    // TODO the set is not big enough! Should start from the top, to accommodate old uncles...
    // TODO check that the cycles are actually prevented by all this...
    val uncles = selectors.selectors.takeWhile(_ != selector)
    Selector.descendants(uncles.toSet)
  }
}
