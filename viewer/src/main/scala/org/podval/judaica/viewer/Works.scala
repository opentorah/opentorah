/*
 *  Copyright 2011-2013 Leonid Dubinsky <dub@podval.org>.
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


  private var works: Option[Set[Work]] = None


  def getByName(name: String): Option[Work] = Names.byName(name, get)


  def get: Set[Work] = {
    if (works.isEmpty) load(textsDirectory)

    works.get
  }


  def load(directory: File) {
    works = Some(new DirectoryScanner(directory).describedDirectories.map(d =>
      Work(d.name, d.metadata, d.directory)))
  }
}
