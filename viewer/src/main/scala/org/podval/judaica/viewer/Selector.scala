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

import scala.xml.Elem

import java.io.File

import Selector.ParsingContext


abstract class Selector(knownSelectors: Set[Selector], xml: Elem) extends Named with Selectors {
  def isNumbered: Boolean
  final def isNamed: Boolean = !isNumbered 
  def asNumbered: NumberedSelector
  def asNamed: NamedSelector

  final override val names = Names(xml)
  final override val selectors = Selector.parse(knownSelectors, xml)

  final def isTerminal: Boolean = selectors.isEmpty


  def parseStructure(context: ParsingContext, selectors: Selectors, xml: Elem): Structure = {
    val nextContext = context.copy(
      isDominant = context.isDominant && selectors.isDominantSelector(this),
      knownSelectors = cousins(selectors))

    xml.attributeOption("file").fold {
      if (isNumbered)
        new NumberedParsedStructure(context, asNumbered, xml)
      else
        new NamedParsedStructure(context, asNamed, xml)
    }{
      fileName: String =>
        val nextParsingFile: File = new File(context.parsingFile.getParentFile, fileName)
        val realNextContext = nextContext.copy(parsingFile = nextParsingFile)

        if (isNumbered)
          new NumberedLazyStructure(realNextContext, asNumbered, xml)
        else
          new NamedLazyStructure(realNextContext, asNamed, xml)
    }
  }


  private def cousins(selectors: Selectors): Set[Selector] = {
    // TODO the set is not big enough! Should start from the top, to accommodate old uncles...
    // TODO check that the cycles are actually prevented by all this...
    val uncles = selectors.selectors.takeWhile(_ != this)
    Selector.descendants(uncles.toSet)
  }
}


final class NumberedSelector(knownSelectors: Set[Selector], xml: Elem) extends Selector(knownSelectors, xml) {
  override def isNumbered: Boolean = true
  override def asNumbered: NumberedSelector = this
  override def asNamed: NamedSelector = throw new ClassCastException
}


final class NamedSelector(knownSelectors: Set[Selector], xml: Elem) extends Selector(knownSelectors, xml) {
  override def isNumbered: Boolean = false
  override def asNumbered: NumberedSelector = throw new ClassCastException
  override def asNamed: NamedSelector = this
}



object Selector {

  case class ParsingContext(
     isDominant: Boolean,
     dominantParentSelection: StructureSelection,
     parsingFile: File,
     knownSelectors: Set[Selector]
  )


  type Xmls = Map[Selector, Elem]


  def descendants(next: Set[Selector]): Set[Selector] = descendants(Set.empty, next)


  private def descendantsOfOne(result: Set[Selector], next: Selector): Set[Selector] = descendants(result, Set(next))


  private def descendants(result: Set[Selector], next: Set[Selector]): Set[Selector] = {
    val add = next -- result
    if (add.isEmpty) result else {
      val children: Set[Selector] = add.flatMap(_.selectors)
      descendants(result ++ next, children)
    }
  }


  def parse(knownSelectors: Set[Selector], xml: Elem): Seq[Selector] =
    Parse.sequence[Elem, Set[Selector], Selector](parseSelector)(descendantsOfOne)(knownSelectors, xml.elemsFilter("selector"))


  private def parseSelector(knownSelectors: Set[Selector], xml: Elem): Selector = {
    def newSelector =
      if (xml.booleanAttribute("isNumbered"))
        new NumberedSelector(knownSelectors, xml)
      else
        new NamedSelector(knownSelectors, xml)

    def referenceToKnownSelector(name: String) = Names.doFind(knownSelectors, name, "selector")

    xml.attributeOption("name").fold(newSelector)(referenceToKnownSelector)
  }
}



trait Selectors {

  import Selectors.Format


  def selectors: Seq[Selector]


  final def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)


  final def getSelectorByName(name: String): Selector = Names.doFind(selectors, name, "selector")


  final def dominantSelector: Selector = selectors.head


  final def isDominantSelector(selector: Selector): Boolean = selector == dominantSelector


  final def dominantFormat: Format =
    if (selectors.isEmpty) Nil else dominantSelector +: dominantSelector.dominantFormat


  final def parseStructures(context: ParsingContext, xml: Elem): Map[Selector, Structure] =
    parseStructures(context, preParseStructures(xml))


  // TODO verify that all structures requested by the selectors are present; some allowed structures need to be calculated...
  // TODO make sure that they are retrievable, too - for instance, week/chapter!
  ///    selectors.foreach(selector => Exists(structures, selector.defaultName, "structures"))
  final def parseStructures(context: ParsingContext, xmls: Selector.Xmls): Map[Selector, Structure] =
    for ((selector, xml) <- xmls) yield selector -> selector.parseStructure(context, this, xml)


  final def preParseStructures(xmls: Elem): Selector.Xmls =
    xmls.elemsFilter("structure").map(xml => getSelectorByName(xml.getAttribute("selector")) -> xml).toMap


  final def formats: Seq[Format] =
    if (selectors.isEmpty) Seq(Nil) else
    selectors.flatMap(selector => selector.formats.map (selector +: _))


  final def parseFormat(formatOption: Option[String]): Format = formatOption.fold(dominantFormat)(parseFormat)


  final def parseFormat(format: String): Format =
    Parse.sequence[String, Selectors, Selector](_.getSelectorByName(_)) ((selectors, selector) => selector) (this, format.split("/"))
}



object Selectors {

  type Format = Seq[Selector]
}
