/*
 * Copyright 2014 Leonid Dubinsky <dub@podval.org>.
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


trait Language {

  def name: String


  def numberToString(number: Int): String


  // TODO introduce gender
  def numberToSpelledOutString(number: Int): String


  // Compose the words in accordance with grammar rules of the language: "chapter three" or "third chapter"...
  def name(nameName: String, name: String): String = nameName + " " + name
}



object Languages {

  private val name2lang: Map[String, Language] = List[Language](HebrewLanguage).map(l => (l.name, l)).toMap


  def get(name: String): Language = name2lang.get(name).getOrElse(HebrewLanguage)
}
