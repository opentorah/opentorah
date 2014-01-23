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


trait Selectors {

  import Selector.Format


  def selectors: Seq[Selector]


  final def selectorByName(name: String): Option[Selector] = Names.find(selectors, name)


  final def getSelectorByName(name: String): Selector = Names.doFind(selectors, name, "selector")


  final def dominantSelector: Selector = selectors.head


  final def isDominantSelector(selector: Selector): Boolean = selector == dominantSelector


  final def dominantFormat: Format = if (selectors.isEmpty) Nil else dominantSelector +: dominantSelector.dominantFormat


  final def formats: Seq[Format] =
    if (selectors.isEmpty) Seq(Nil) else
      selectors.flatMap(selector => selector.formats.map (selector +: _))


  // TODO segregate into some kind of a parser object...


  final def parseFormat(formatOption: Option[String]): Format = formatOption.fold(dominantFormat)(parseFormat)


  final def parseFormat(format: String): Format =
    Parse.sequence[String, Selectors, Selector](_.getSelectorByName(_)) ((selectors, selector) => selector) (this, format.split("/"))
}
