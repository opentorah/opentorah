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


trait Structure extends Named with Ordering[Div] {

  val selector: Selector


  final override def names = selector.names


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


  final def divByNumber(number: Int): Option[Div] = {
    if ((number < 1) || (number > length)) None else {
      if (number <= divs.length) Some(divs(number-1)) else {
        // TODO check that this is a dominant structure - or rely on the (not yet implemented) checks during parsing that the structures are complete :)
        if (isNumbered && selector.isTerminal) Some(new GeneratedTerminalDominantNumberedDiv(this, number)) else None
      }
    }
  }


  final def getDivByNumber(number: Int): Div = Exists(divByNumber(number), number.toString, "div")


  // TODO check that named or non-dominant structure is fully defined
  // TODO maybe structure for the terminal selectors shouldn't be *allowed*, not just allowed to be omitted...
  // TODO length should really be supplied fo rthe terminal structure!
  protected final def checkLength[T <: Div](divs: Seq[T]): Seq[T] = {
    if (!selector.isTerminal && lengthOption.isDefined && lengthOption.get != divs.length)
      throw new ViewerException(s"Wrong length: expected ${lengthOption.get} but got ${divs.length}")

    divs
  }
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
