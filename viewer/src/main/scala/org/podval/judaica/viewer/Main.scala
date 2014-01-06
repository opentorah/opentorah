/*
 *  Copyright 2013 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.xml.{Xml, Load, Html}
import Xml.XmlOps
import org.podval.judaica.display.{ElementDisplayer, TanachDisplayers}

import java.io.File


/*
    This is for experiments with transforming the XML files for display.
    When properly generalized to be driven by the metadata, this will go away.
 */

object Main {



  def main(args: Array[String]) {
    val xml = Load.loadFile(new File("/home/dub/Code/judaica/texts/Tanach/Jerusalem/Genesis.xml"))
    val output = new File("/home/dub/Code/judaica/tmp/Genesis.html")
//    val result = TanachStructure.book.displayDiv(xml, Set.empty)
    val result = ElementDisplayer.find(xml, TanachDisplayers.displayers).display(xml, TanachDisplayers.displayers)
    Html.html("tanach", result).print(output)
  }
}
