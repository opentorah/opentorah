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

package org.podval.judaica.importers
package tanach

import org.podval.judaica.common.Xml
import Xml.getAttribute

import scala.xml.{Node, Elem}


abstract class TanachImporter(inputDirectory: String, outputDirectory: String)
    extends Importer(inputDirectory, outputDirectory)
{

  final def run {
    output2inputName foreach { case (inputName, outputName) => importBook(inputName, outputName) }
  }


  protected def output2inputName: Map[String, String]


  protected final override def processBook(xml: Node, outputName: String): Node = {
    val breaks =
      Xml.loadResource(classOf[TanachImporter], outputName, "meta")
        .child
        .groupBy(getAttribute("chapter"))
        .mapValues(_.groupBy(getAttribute("verse"))) // TODO drop coordinates here with .mapValues?

    transformDiv(xml, "book") { flatMapChildren(_, {
      transformDiv(_, "chapter") { chapter => flatMapChildren(chapter, {
        transformDiv(_, "verse") { verse =>
          breaks
          .getOrElse(getAttribute(chapter, "n"), Map.empty)
          .getOrElse(getAttribute(verse, "n"), Seq.empty)
          .map (dropCoordinates(_)) ++
          verse
        }
      })}
    })}(0)
  }


  private def transformDiv(node: Node, divType: String)(f: Node => Seq[Node]) =
    if (!Xml.isDiv(node, divType)) node else f(node)


  private def flatMapChildren(node: Node, f: Node => Seq[Node]): Elem =
    node.asInstanceOf[Elem].copy(child = node.child flatMap f)


  private def dropCoordinates(break: Node): Node = {
    // TODO drom "chapter" and "verse" attributes
    break
  }
}
