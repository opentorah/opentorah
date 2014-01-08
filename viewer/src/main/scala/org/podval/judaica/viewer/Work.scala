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

import org.podval.judaica.xml.Xml.XmlOps

import scala.xml.Elem

import java.io.File


// TODO do weak references and lazy (re-)load!!!


object Works extends ByName[Work] {

  val textsDirectory = new File("/home/dub/Code/judaica/texts/")


  override lazy val named: Seq[Work] =
    DirectoryScanner.describedDirectories(textsDirectory).map(d => new Work(d.name, d.metadata, d.directory))
}



final class Work(name: String, metadata: Elem, val directory: File) extends Named {

  override val names = Names(name, metadata)


  val selectors: Selectors = Selectors(metadata)


  val structures: Structures = Structures(selectors, metadata.oneChild("structures"))  // TODO make structures optional?


  lazy val editions = new Editions(this)


  override def toString: String = "Work (" + directory + ") " + names
}
