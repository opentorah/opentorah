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


object Exists {

  def apply[T](value: Option[T], name: String, what: String): T = {
    require(!value.isEmpty, s"$what $name does not exist")
    value.get
  }


  def apply[T](value: Seq[T], what: String): Seq[T] = {
    require(!value.isEmpty, s"$what does not exist")
    value
  }
}
