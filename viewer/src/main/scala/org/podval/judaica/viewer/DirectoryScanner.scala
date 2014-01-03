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

import org.podval.judaica.xml.Load

import scala.xml.Elem

import java.io.File


object DirectoryScanner {

  case class DescribedDirectory(name: String, directory: File, metadata: Elem)


  def describedDirectories(directory: File): Seq[DescribedDirectory] = {
    require(directory.isDirectory)

    directory.listFiles.toSeq.filter(_.isDirectory).map { subdirectory =>
      val name = subdirectory.getName
      metadata(name, directory, subdirectory) map (DescribedDirectory(name, subdirectory, _))
    }.flatten
  }


  def metadata(name: String, directory: File, subdirectory: File): Option[Elem] = {
    val metadataFileInParent = new File(directory, name + ".xml")
    val metadataFileInSubdirectory = new File(subdirectory, "index.xml")
    if (!metadataFileInParent.exists && !metadataFileInSubdirectory.exists) None else {
      val file = if (metadataFileInSubdirectory.exists) metadataFileInSubdirectory else metadataFileInParent
      val metadata = Load.loadFile(file, "index")
      Some(metadata)
    }
  }
}
