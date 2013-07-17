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

import org.podval.judaica.xml.{Xml, Div, Load}
import Xml.getAttribute

import scala.xml.{Node, Elem}


abstract class TanachImporter(inputDirectory: String, outputDirectory: String)
    extends Importer(inputDirectory, outputDirectory)
{

  final def run {
    output2inputName foreach { case (inputName, outputName) => importBook(inputName, outputName) }
  }


  protected def output2inputName: Map[String, String]


  protected final override def processBook(xml: Elem, outputName: String): Elem = {
    // TODO write a merge function - and reformat the metadata accordingly?
    val breaks =
      Xml.elems(Load.loadResource(classOf[TanachImporter], outputName, "meta"))
        .groupBy(getAttribute("chapter"))
        .mapValues(_.groupBy(getAttribute("verse")).mapValues(_.map(dropChapterAndVerse)))

    transformDiv(xml, "book") { flatMapChildren(_, {
      transformDiv(_, "chapter") { chapter => flatMapChildren(chapter, {
        transformDiv(_, "verse") { verse =>
          val prefixBreaks: Seq[Elem] = breaks
            .getOrElse(getAttribute(chapter, "n"), Map.empty)
            .getOrElse(getAttribute(verse, "n"), Seq.empty)

          val result: Seq[Elem] = prefixBreaks ++ Seq[Elem](verse)
          result
        }
      })}
    })}(0).asInstanceOf[Elem]   // TODO get rid of the cast!!!
  }


  private def transformDiv(elem: Elem, divType: String)(f: Elem => Seq[Node]): Seq[Node] = elem match {
    case e@Div(divType) => f(e)
    case e => Seq(elem)
  }


  private def flatMapChildren(elem: Elem, f: Elem => Seq[Node]): Elem = elem.copy(child = Xml.elems(elem) flatMap f)


  private def dropChapterAndVerse(break: Elem): Elem =
    break.copy(attributes = break.attributes.filter(a => TanachImporter.chapterAndVerse.contains(a.key)))
}


object TanachImporter {

  val chapterAndVerse = Set("chapter", "verse")
}