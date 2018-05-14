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

package org.podval.judaica.viewer


trait Structure extends Named with Ordering[Div] {

  val selector: Selector

  final override def names: Names = selector.names

  def isRoot: Boolean

  def asNonRoot: NonRootStructure

  final def isNumbered: Boolean = selector.isNumbered

  final def isNamed: Boolean = selector.isNamed

  def asNumbered: NumberedStructure

  def asNamed: NamedStructure

  final def isTerminal: Boolean = selector.isTerminal

  def divs: Seq[Div]

  final def length: Int = lengthOption.getOrElse(divs.length)

  protected val lengthOption: Option[Int]

  final override def compare(x: Div, y: Div): Int = {
    require(x.structure == this && y.structure == this)
    require(divs.contains(x) && divs.contains(y))
    divs.indexOf(x) - divs.indexOf(y)
  }

  final def divById(id: String): Option[Div] = {
    val numberOption: Option[Int] = try { Some(id.toInt) } catch { case e: NumberFormatException => None }

    numberOption.fold {
      if (isNumbered) throw new ViewerException(s"$id is not a number")
      asNamed.divByName(id) : Option[Div]
    } {
      divByNumber
    }
  }

  final def divByNumber(number: Int): Option[Div] = {
    if ((number < 1) || (number > length)) None else {
      if (number <= divs.length) Some(divs(number-1)) else {
        // TODO check that this is a dominant structure - or rely on the (not yet implemented) checks during parsing that the structures are complete :)
        if (isNumbered && selector.isTerminal) Some(new GeneratedTerminalDominantNumberedDiv(this.asNumbered, number)) else None
      }
    }
  }

  final def getDivByNumber(number: Int): Div = Exists(divByNumber(number), number.toString, "div")

  final override def toString: String = s"Structure $defaultName[$length]"
}


trait NonRootStructure extends Structure {
  final override def isRoot: Boolean = false

  final override def asNonRoot: NonRootStructure = this

  val parentDiv: Div
}


trait NamedStructure extends Structure {
  override val selector: NamedSelector

  final override def asNumbered: NumberedStructure = throw new ClassCastException

  final override def asNamed: NamedStructure = this

  override def divs: Seq[NamedDiv]

  final def divByName(name: String): Option[NamedDiv] = Names.find(divs, name)

  final def getDivByName(name: String): NamedDiv = Names.doFind(divs, name, "div")
}


trait NumberedStructure extends Structure {
  override val selector: NumberedSelector

  final override def asNumbered: NumberedStructure = this

  final override def asNamed: NamedStructure = throw new ClassCastException

  override def divs: Seq[NumberedDiv]
}
