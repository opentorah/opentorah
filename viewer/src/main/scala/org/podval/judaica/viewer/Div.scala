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
trait Div extends Selectors with Ordered[Div] {

  def structures: Map[Selector, Structure]


  final def getStructure(selector: Selector): Structure = structures(selector)


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



trait TerminalDominantDiv extends DominantDiv {

  final override def selectors: Seq[Selector] = Seq.empty


  final override def structures: Map[Selector, Structure] = Map.empty


  final override def dominantStructure: Structure = throw new UnsupportedOperationException
}



final class GeneratedTerminalDominantNumberedDiv(override val structure: Structure, override val number: Int)
  extends TerminalDominantDiv with NumberedDiv
