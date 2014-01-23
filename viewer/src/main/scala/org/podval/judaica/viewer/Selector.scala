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


trait Selector extends Named with Selectors {

  def isNumbered: Boolean


  final def isNamed: Boolean = !isNumbered


  def asNumbered: NumberedSelector


  def asNamed: NamedSelector


  final def isTerminal: Boolean = selectors.isEmpty
}



trait NumberedSelector extends Selector {

  final override def isNumbered: Boolean = true


  final override def asNumbered: NumberedSelector = this


  final override def asNamed: NamedSelector = throw new ClassCastException
}



trait NamedSelector extends Selector {

  final override def isNumbered: Boolean = false


  final override def asNumbered: NumberedSelector = throw new ClassCastException


  final override def asNamed: NamedSelector = this
}



object Selector {

  type Format = Seq[Selector]


  def descendants(next: Set[Selector]): Set[Selector] = descendants(Set.empty, next)


  def descendants(result: Set[Selector], next: Set[Selector]): Set[Selector] = {
    val add = next -- result
    if (add.isEmpty) result else {
      val children: Set[Selector] = add.flatMap(_.selectors)
      descendants(result ++ next, children)
    }
  }
}
