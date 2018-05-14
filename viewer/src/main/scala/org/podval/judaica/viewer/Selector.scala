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


trait Selector extends Named with Selectors {
  /**
   * To include the name of the selector in the <head> element of the corresponding <div> or not?
   * For example, for chapters, the <head> contains "Chapter number", but for verses - just the number.
   *
   * @return
   */
  def isNameIncludedInHead: Boolean

  /**
   * What goes before the name of the Div in the <head>.
   * For example, names of aliyos are in brackets: "[third aliya]".
   *
   * @return
   */
  def headPrefix: Option[String]
  def headSuffix: Option[String]

  def isNumbered: Boolean

  final def isNamed: Boolean = !isNumbered

  def asNumbered: NumberedSelector

  def asNamed: NamedSelector

  final def isTerminal: Boolean = selectors.isEmpty

  final def name(lang: Language, divName: String): String = {
    val nameNameOption = if (isNameIncludedInHead) names.byLang(lang) else None
    headPrefix.getOrElse("") +
    (if (nameNameOption.isDefined) lang.name(nameNameOption.get.name, divName) else divName) +
    headSuffix.getOrElse("")
  }
}


trait NumberedSelector extends Selector {
  final override def isNumbered: Boolean = true

  final override def asNumbered: NumberedSelector = this

  final override def asNamed: NamedSelector = throw new ClassCastException

  /**
   * Present the number as number - or as words?
   * For example, foe chapters and verses it is just a number, but for aliyos - words ("first").
   *
   * @return
   */
  def isSpelledOut: Boolean

  final def divName(lang: Language, div: NumberedDiv): String =
    name(lang, (if (isSpelledOut) lang.numberToSpelledOutString _ else lang.numberToString _)(div.number))
}


trait NamedSelector extends Selector {
  final override def isNumbered: Boolean = false

  final override def asNumbered: NumberedSelector = throw new ClassCastException

  final override def asNamed: NamedSelector = this

  final def divName(lang: Language, div: NamedDiv): String = {
    val nameOption = div.names.byLang(lang)
    name(lang, if (nameOption.isDefined) nameOption.get.name else "")
  }
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
