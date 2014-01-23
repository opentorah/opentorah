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
import ParseException.withMetadataFile

import scala.xml.Elem

// TODO I think I can avoid passing around object that are not completely parsed by introducing temporary objects
// that provide access methods to the information that was already parsed...
// If this is cleaned and can be made to work everywhere, is XmlFile cache is still useful?
// (by the way, shouldn't be "release" method introduced on it?)

object StructureParser {

  private abstract class ParseableStructure(override val selector: Selector, xml: Elem) extends Structure {

    protected final override val lengthOption: Option[Int] = xml.intAttributeOption("length")


    protected final def open(xml: Elem): Elem = {
      val result = xml.oneChild("structure")
      // TODO check that this is the correct structure :)
      result
    }


    protected final def divs(xml: Elem): Seq[Elem] = xml.elemsFilter("div")
  }



  private abstract class ParsedNamedStructure(override val selector: NamedSelector, xml: Elem)
    extends ParseableStructure(selector, xml) with NamedStructure
  {
    protected final def parseDivs(context: ParsingContext, xml: Elem): Seq[NamedDiv] =
      checkLength(divs(xml).map(xml => DivParser.parseNamed(context, this, xml)))
  }



  private final class NamedParsedStructure(context: ParsingContext, selector: NamedSelector, xml: Elem)
    extends ParsedNamedStructure(selector, xml)
  {
    override val divs: Seq[NamedDiv] = parseDivs(context, xml)
  }



  private final class NamedLazyStructure(context: ParsingContext, selector: NamedSelector, xml: Elem)
    extends ParsedNamedStructure(selector, xml)
  {
    override def divs: Seq[NamedDiv] = divs_.get


    private[this] val divs_ = LazyLoad(withMetadataFile(context.parsingFile)(xml => parseDivs(context, open(xml))))
  }



  private abstract class ParseableNumberedStructure(override val selector: NumberedSelector, xml: Elem)
    extends ParseableStructure(selector, xml) with NumberedStructure
  {
    protected final def parseDivs(context: ParsingContext, xml: Elem): Seq[NumberedDiv] =
      checkLength(divs(xml).zipWithIndex.map { case (xml, num) => DivParser.parseNumbered(context, this, num+1, xml) })
  }



  private final class NumberedParsedStructure(context: ParsingContext, selector: NumberedSelector, xml: Elem)
    extends ParseableNumberedStructure(selector, xml)
  {
    // TODO check that the Paths for the non-dominant structure are properly ordered; same for Named
    override val divs: Seq[NumberedDiv] = parseDivs(context, xml)
  }



  private final class NumberedLazyStructure(context: ParsingContext, selector: NumberedSelector, xml: Elem)
    extends ParseableNumberedStructure(selector, xml)
  {
    override def divs: Seq[NumberedDiv] = divs_.get


    private[this] val divs_ = LazyLoad(withMetadataFile(context.parsingFile)(xml => parseDivs(context, open(xml))))
  }


  def parseStructure(context: ParsingContext, selector: Selector, selectors: Selectors, xml: Elem): Structure = {
    val nextContext = context.copy(
      isDominant = context.isDominant && selectors.isDominantSelector(selector),
      knownSelectors = cousins(selector, selectors))

    xml.attributeOption("file").fold {
      if (selector.isNumbered)
        new NumberedParsedStructure(context, selector.asNumbered, xml)
      else
        new NamedParsedStructure(context, selector.asNamed, xml)
    }{
      fileName: String =>
        import java.io.File

        val nextParsingFile: File = new File(context.parsingFile.getParentFile, fileName)
        val realNextContext = nextContext.copy(parsingFile = nextParsingFile)

        if (selector.isNumbered)
          new NumberedLazyStructure(realNextContext, selector.asNumbered, xml)
        else
          new NamedLazyStructure(realNextContext, selector.asNamed, xml)
    }
  }


  private def cousins(selector: Selector, selectors: Selectors): Set[Selector] = {
    // TODO the set is not big enough! Should start from the top, to accommodate old uncles...
    // TODO check that the cycles are actually prevented by all this...
    val uncles = selectors.selectors.takeWhile(_ != selector)
    Selector.descendants(uncles.toSet)
  }
}
