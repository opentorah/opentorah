/*
 *  Copyright 2011-2014 Leonid Dubinsky <dub@podval.org>.
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


// TODO do weak references and lazy (re-)load!!!


object Works {

  val textsDirectory = new File("/home/dub/Code/judaica/texts/")


  lazy val works: Set[Work] = new DirectoryScanner(textsDirectory).describedDirectories.map(d => new Work(d.name, d.metadata, d.directory))


  def getByName(name: String): Option[Work] = Names.byName(name, works)
}
