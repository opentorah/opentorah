/*
 * Copyright 2012 Podval Group.
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

import org.podval.judaica.common.Xml.{loadFile, getAttribute}

import java.io.File


final class WorkImpl private(
  override val names: Names,
  val directory: String) extends Work
{

  override def editions: Set[Edition] = throw new UnsupportedOperationException // XXX


  def defaultEdition: Edition = throw new UnsupportedOperationException // XXX


  override def toString: String = "Work (" + directory + ") " + names
}


object WorkImpl {

  def apply(file: File): WorkImpl = {
    val node = loadFile(file, "work")

    new WorkImpl(
      Names(node),
      getAttribute(node, "directory")
    )
  }
}
