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

import scala.xml.Elem


abstract class Selection(val work: Work, val editions: Editions, val path: Selection.Path) {

  def isStructure: Boolean
  def isDiv: Boolean
  def asStructure: StructureSelection
  def asDiv: DivSelection

  def structure: Structure

  def select(name: String): Selection
  def selectDominant(name: String): Selection

  def selectDiv(div: Div): StructureSelection = new StructureSelection(work, editions, path :+ div)


  def selectPath(what: String): Selection = what.split("/").foldLeft(this)(_.select(_))
  def selectDominantPath(what: String): Selection = what.split("/").foldLeft(this)(_.selectDominant(_))
}



final class StructureSelection(work: Work, editions: Editions, path: Seq[Div]) extends Selection(work, editions, path) {

  override def isStructure: Boolean = true
  override def isDiv: Boolean = false
  override def asStructure: StructureSelection = this
  override def asDiv: DivSelection = throw new ClassCastException

  def structures: Structures = if (path.isEmpty) work else path.last
  override def structure: Structure = path.last.structure

  override def select(name: String): DivSelection = selectStructure(name)
  def selectStructure(name: String): DivSelection = selectStructure(structures.getSelectorByName(name))
  def selectStructure(selector: Selector): DivSelection = selectStructure(structures.getStructure(selector))
  def selectStructure(structure: Structure): DivSelection = new DivSelection(work, editions, path, structure)

  override def selectDominant(name: String): DivSelection = selectDominantStructure(name)
  def selectDominantStructure(name: String): DivSelection = selectDominantStructure(structures.getSelectorByName(name))
  def selectDominantStructure(selector: Selector): DivSelection = {
    if (!structures.isDominantSelector(selector)) throw new ViewerException(s"Selector $selector is not dominant")
    selectStructure(if (path.isEmpty) structures.getStructure(selector) else path.last.asDominant.dominantStructure)
  }


  def parseDominantPath(path: String): Selection.Path = {
    val target = selectDominantPath(path)
    if (!target.isStructure) throw new ViewerException(s"Path $path ended with a Div selection")
    if (!target.structure.selector.isTerminal) throw new ViewerException(s"Path $path is incomplete")
    target.path.drop(this.path.length)
  }


  def xmlContent: Elem = content(None, false)
  def xmlContent(format: String): Elem = content(Some(format), false)


  def htmlContent: Elem = content(None, true)
  def htmlContent(format: String): Elem = content(Some(format), true)


  def content(formatOption: Option[String], isHtml: Boolean): Elem =
    content(structures.parseFormat(formatOption), isHtml)


  def content(format: Seq[Selector], isHtml: Boolean): Elem = {
    val result = content(format)
    if (isHtml) toHtml(result) else result
  }


  def content(format: Seq[Selector]): Elem = editions.content(path, format)


  def toHtml(xml: Elem): Elem = ??? // TODO
}



final class DivSelection(work: Work, editions: Editions, path: Seq[Div], override val structure: Structure) extends Selection(work, editions, path) {

  override def isStructure: Boolean = false
  override def isDiv: Boolean = true
  override def asStructure: StructureSelection = throw new ClassCastException
  override def asDiv: DivSelection = this


  def divs: Seq[Div] = structure.divs


  override def select(name: String): StructureSelection = selectDiv(name)
  override def selectDominant(name: String): StructureSelection = selectDiv(name)


  def selectDiv(name: String): StructureSelection = {
    val numberOption: Option[Int] = try { Some(name.toInt) } catch { case e: NumberFormatException => None }

    val div: Div =
      if (numberOption.isDefined) structure.getDivByNumber(numberOption.get) else
      if (structure.isNumbered) throw new ViewerException(s"$name is not a number") else
      structure.asNamed.getDivByName(name)

    selectDiv(div)
  }
}



object Selection {

  type Path = Seq[Div]


  def apply(workName: String): StructureSelection = apply(Works.getWorkByName(workName))

  def apply(work: Work): StructureSelection = apply(work, NoEditions)

  def apply(workName: String, editionNames: String): StructureSelection = apply(Works.getWorkByName(workName), editionNames)

  def apply(work: Work, editionNames: String): StructureSelection = apply(work, Editions(work, editionNames))

  def apply(work: Work, editions: Editions): StructureSelection = new StructureSelection(work, editions, Seq.empty)
}
