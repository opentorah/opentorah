/*
 *  Copyright 2013-2018 Leonid Dubinsky <dub@podval.org>.
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

import java.io.File


trait Edition extends Named {
  val work: Work

  val directory: File

  override val names: Names

  val language: String

  def storage: DirectoryStorage

  final def stylesheet: File = new File(directory, "stylesheet.css")

  final def content(div: Div, format: Selector.Format): Content = {
    val unbound = storage.content(div.path.tail, format)
    val bound = DivContent.bindWithThis(unbound, div.path.head, Languages.get(language))
    bound
  }
}
