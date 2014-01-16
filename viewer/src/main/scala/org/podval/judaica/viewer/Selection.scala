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


abstract class Selection(val work: Work, val editions: Editions, val path: Seq[Div]) {

  def isStructure: Boolean
  def isDiv: Boolean
  def asStructure: StructureSelection
  def asDiv: DivSelection

  def step(name: String): Selection

  def steps(what: String): Selection = what.split("/").foldLeft(this)(_.step(_))
}



final class StructureSelection(work: Work, editions: Editions, path: Seq[Div]) extends Selection(work, editions, path) {

  override def isStructure: Boolean = true
  override def isDiv: Boolean = false
  override def asStructure: StructureSelection = this
  override def asDiv: DivSelection = throw new ClassCastException


  def structures: Structures = if (path.isEmpty) work else path.last

  def structure(name: String): DivSelection = new DivSelection(work, editions, path, structures.getStructureByName(name))

  override def step(name: String): DivSelection = structure(name)

  def div(structureName: String, divName: String): StructureSelection = structure(structureName).div(divName)

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


  def toHtml(xml: Elem): Elem = ???
}



final class DivSelection(work: Work, editions: Editions, path: Seq[Div], structure: Structure) extends Selection(work, editions, path) {

  override def isStructure: Boolean = false
  override def isDiv: Boolean = true
  override def asStructure: StructureSelection = throw new ClassCastException
  override def asDiv: DivSelection = this


  def divs: Seq[Div] = structure.divs


  def div(name: String): StructureSelection = {
    val numberOption: Option[Int] = try { Some(name.toInt) } catch { case e: NumberFormatException => None }

    val div: Div =
      if (numberOption.isDefined) structure.getDivByNumber(numberOption.get) else
      if (structure.isNumbered) throw new ViewerException(s"$name is not a number") else
      structure.asNamed.getDivByName(name)

    new StructureSelection(work, editions, path :+ div)
  }


  override def step(name: String): StructureSelection = div(name)
}



object Selection {

  def apply(workName: String): StructureSelection = apply(Works.getWorkByName(workName))

  def apply(work: Work): StructureSelection = apply(work, NoEditions)

  def apply(workName: String, editionNames: String): StructureSelection = apply(Works.getWorkByName(workName), editionNames)

  def apply(work: Work, editionNames: String): StructureSelection = apply(work, Editions(work, editionNames))

  def apply(work: Work, editions: Editions): StructureSelection = new StructureSelection(work, editions, Seq.empty)
}
