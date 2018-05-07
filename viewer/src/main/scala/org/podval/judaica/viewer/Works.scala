/*
 * Copyright 2012-2014 Leonid Dubinsky <dub@podval.org>.
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


object Works {

  val directory = new File("/home/dub/Projects/judaica/data/texts/")


  def works: Seq[Work] = works_.get


  private[this] val works_ = LazyLoad(DirectoryScanner(directory, WorkParser.parseWork))


  def workByName(name: String): Option[Work] = Names.find(works, name)


  def getWorkByName(name: String): Work = Names.doFind(works, name, "work")


  def stylesheet: File = new File(directory, "stylesheet.css")
}
