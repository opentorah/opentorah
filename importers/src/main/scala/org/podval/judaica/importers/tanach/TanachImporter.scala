/*
 *  Copyright 2011 Leonid Dubinsky <dub@podval.org>.
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

import org.podval.judaica.common.Xml.{loadResource, getAttribute}

import scala.xml.{Node, Elem}


abstract class TanachImporter(inputDirectory: String, outputDirectory: String)
    extends Importer(inputDirectory, outputDirectory)
{

    final def run {
        output2inputName foreach { case (inputName, outputName) => importBook(inputName, outputName) }
    }


    protected def output2inputName: Map[String, String]
  

    protected final override def getStylesheet = "tanach"


    protected final override def processBook(xml: Node, outputName: String): Node = {
        val breaks =
            loadResource(classOf[TanachImporter], outputName, "meta")
            .child
            .groupBy(getAttribute("chapter"))
            .mapValues(_.groupBy(getAttribute("verse")))

        xml match { case b =>
            if (!isDiv(b, "book")) b else { replaceChildren(b, b.child map { c =>
                if (!isDiv(c, "chapter")) c else { replaceChildren(c, c.child flatMap { v =>
                    val verseBreaks: Seq[Node] =
                        if (!isDiv(v, "verse"))
                            Seq[Node]()
                        else
                            breaks
                            .getOrElse(getAttribute(c, "n"), Map[String, Seq[Node]]())
                            .getOrElse(getAttribute(v, "n"), Seq[Node]())

                    verseBreaks ++ Seq(v)
                })}
            })}
        }
    }


    def isDiv(node: Node, divType: String): Boolean =
        node.isInstanceOf[Elem] && node.asInstanceOf[Elem].label == "div" && (getAttribute(node, "type") == divType)


    def replaceChildren(node: Node, children: Seq[Node]): Elem =
        node.asInstanceOf[Elem].copy(child = children)
}
