/*
 *  Copyright 2011-2013 Leonid Dubinsky <dub@podval.org>.
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  under the License.
 */

package org.podval.judaica.importers.tanach.jerusalem


final class Line(var line: String) {

  def isEmpty = line.isEmpty


  def size = line.size


  def indexOf(what: String) = line.indexOf(what)


  def consumeToSpace(): String = {
    consumeToIndex(line.indexOf(" "))
  }


  def consumeBracketed(): Option[String] = {
    if (line.startsWith("[")) {
      val index = line.indexOf("]")
      val bracketed = consumeToIndex(index+1)
      val result = bracketed.drop(1).dropRight(1)
      Some(result)
    } else {
      None
    }
  }


  def consume(what: String): Boolean = {
    val result = line.startsWith(what)
    if (result) {
      consumeToIndex(what.length())
    }
    result
  }


  def consumeToIndex(index: Int): String = {
    val result = line.take(index)
    // @todo get rid of the trim?
    line = line.drop(index).trim()
    result
  }
}
