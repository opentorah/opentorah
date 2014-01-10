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

import org.podval.judaica.xml.Xml.XmlOps

import scala.xml.Elem


abstract class Structure(val selector: Selector, val divs: Seq[Div]) extends Named {

  final override def names = selector.names
}


abstract class Selector(override val names: Names, val selectors: Seq[Selector]) extends Named {
  def isNumbered: Boolean
  def asNumbered: NumberedSelector
  def asNamed: NamedSelector

  def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)
}


abstract class Div(val selectors: Seq[Selector], val structures: Seq[Structure]) {
  def isNumbered: Boolean
  def asNumbered: NumberedDiv
  def asNamed: NamedDiv
}



final class NamedStructure(
  override val selector: NamedSelector,
  override val divs: Seq[NamedDiv]) extends Structure(selector, divs)


final class NumberedStructure(
  override val selector: NumberedSelector,
  override val divs: Seq[NumberedDiv]) extends Structure(selector, divs)   // TODO something with known length, not Seq...
{
}



final class NumberedSelector(names: Names, selectors: Seq[Selector]) extends Selector(names, selectors) {
  override def isNumbered: Boolean = true
  override def asNumbered: NumberedSelector = this
  override def asNamed: NamedSelector = throw new ClassCastException
}


final class NamedSelector(names: Names, selectors: Seq[Selector]) extends Selector(names, selectors) {
  override def isNumbered: Boolean = false
  override def asNumbered: NumberedSelector = throw new ClassCastException
  override def asNamed: NamedSelector = this
}



final class NumberedDiv(val number: Int, selectors: Seq[Selector], structures: Seq[Structure]) extends Div(selectors, structures) {
  def isNumbered: Boolean = true
  def asNumbered: NumberedDiv = this
  def asNamed: NamedDiv = throw new ClassCastException

}


final class NamedDiv(override val names: Names, selectors: Seq[Selector], structures: Seq[Structure]) extends Div(selectors, structures) with Named {
  def isNumbered: Boolean = false
  def asNumbered: NumberedDiv = throw new ClassCastException
  def asNamed: NamedDiv = this
}
