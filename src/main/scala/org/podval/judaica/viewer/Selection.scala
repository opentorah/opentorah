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


abstract class Selection(val work: Work, val editions: Editions, val div: Div) {

  def isStructure: Boolean
  def isDiv: Boolean
  def asStructure: StructureSelection
  def asDiv: DivSelection

  def structure: Structure

  def select(name: String): Selection
  def selectDominant(name: String): Selection

  def selectDiv(div: Div): StructureSelection = new StructureSelection(work, editions, div)


  def selectPath(what: String): Selection = what.split("/").foldLeft(this)(_.select(_))
  def selectDominantPath(what: String): Selection = what.split("/").foldLeft(this)(_.selectDominant(_))
}



final class StructureSelection(work: Work, editions: Editions, div: Div) extends Selection(work, editions, div) {

  override def isStructure: Boolean = true
  override def isDiv: Boolean = false
  override def asStructure: StructureSelection = this
  override def asDiv: DivSelection = throw new ClassCastException

  override def structure: Structure = div.structure

  override def select(name: String): DivSelection = selectStructure(name)
  def selectStructure(name: String): DivSelection = selectStructure(div.getSelectorByName(name))
  def selectStructure(selector: Selector): DivSelection = selectStructure(div.getStructure(selector))
  def selectStructure(structure: Structure): DivSelection = new DivSelection(work, editions, div, structure)

  override def selectDominant(name: String): DivSelection = selectDominantStructure(name)
  def selectDominantStructure(name: String): DivSelection = selectDominantStructure(div.getSelectorByName(name))
  def selectDominantStructure(selector: Selector): DivSelection = {
    if (!div.isDominantSelector(selector)) throw new ViewerException(s"Selector $selector is not dominant")
    selectStructure(div.asDominant.dominantStructure)
  }


  def parseDominantPath(path: String): Div = {
    val target = selectDominantPath(path)
    if (!target.isStructure) throw new ViewerException(s"Path $path ended with a Div selection")
    if (!target.structure.selector.isTerminal) throw new ViewerException(s"Path $path is incomplete")
    target.div
  }


  def content(formatOption: Option[String]): Content = content(div.parseFormat(formatOption))


  def content(format: Selector.Format): Content = editions.content(div, format)
}



final class DivSelection(work: Work, editions: Editions, div: Div, override val structure: Structure) extends Selection(work, editions, div) {

  override def isStructure: Boolean = false
  override def isDiv: Boolean = true
  override def asStructure: StructureSelection = throw new ClassCastException
  override def asDiv: DivSelection = this


  def divs: Seq[Div] = structure.divs


  override def select(name: String): StructureSelection = selectDiv(name)
  override def selectDominant(name: String): StructureSelection = selectDiv(name)


  def selectDiv(name: String): StructureSelection = selectDiv(Exists(structure.divById(name), name, "id"))
}



object Selection {

  def apply(workName: String): StructureSelection = apply(Works.getWorkByName(workName))


  def apply(work: Work): StructureSelection = apply(work, NoEditions)


  def apply(workName: String, editionNames: String): StructureSelection = apply(Works.getWorkByName(workName), editionNames)


  def apply(work: Work, editionNames: String): StructureSelection = apply(work, Editions(work, editionNames))


  def apply(work: Work, editions: Editions): StructureSelection = new StructureSelection(work, editions, work)
}
