/*
 * Copyright 2010 dub.
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
 * under the License.
 */

package org.podval.judaica.viewer

import scala.xml.Node


final class Viewer(
    format: TextFormat,
    selectionDivType: String,
    texts: Seq[Text])
{
    private val merger = new Merger(format.divTypes)


    def get(postUrl: String): Seq[Node] = {
        val merged = merger.merge(texts.head, texts.tail)

        val formatter = new Formatter(format, selectionDivType, texts, merged)

        formatter.format(postUrl)
    }


    def put(form : Map[String, List[String]]): String = {
        unique(form).toString
    }


    private def unique(form: Map[String, List[String]]): Map[String, String] = {
        for (entry <- form) yield {
            val name = entry._1
            val values = entry._2
            if (values.size > 1) {
                
            }

            name -> values(0)
        }
    }
}
