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


class Formatter(
    texts: Seq[TextDescriptor],
    what: String,
    formatNonStructural: Node => Node,
    formatContent: Node => Seq[Node],
    formatEditable: Node => String)
{
    def format(nodes: Seq[Node]): Seq[Node] = {
        format(nodes, "")
    }


    private def format(nodes: Seq[Node], name: String): Seq[Node] = {
        for (node <- nodes) yield {
            if (!Util.isDiv(node)) {
                formatNonStructural(node)
            } else {
                formatStructurally(node, name)
            }
        }
    }


    private def formatStructurally(node: Node, name: String) = {
        val type_ = Util.getType(node)
        val name_ = Util.getName(node)
        val newName = name + "-" + type_ + "-" + name_
        
        <div class={type_}>
            <span class={type_ +"-name"}>{name_}</span>
            {
                if (Util.isDivType(node, what)) {
                    formatTable(node, newName)
                } else {
                    format(node.child, newName)
                }
            }
        </div>
    }


    private def formatTable(node: Node, name: String) = {
        <table>
            <tr>
            {
                for (text <- texts) yield {
                    <td><span class ="text-name">{text.name}</span></td>
                }
            }
            </tr>
            {
                var numRow = 0
                for (row <- node.child) yield {
                    if (row.label != "merge") {
                        throw new IllegalArgumentException("Not a 'merge'!")
                    }

                    val content = row.child

                    if (content.size != texts.size) {
                        throw new IllegalArgumentException("Wrong length")
                    }

                    numRow += 1
                    <tr>
                    {
                        content.zip(texts).map(formatCell(name + "-" + numRow))
                    }
                    </tr>
                }
            }
        </table>
    }


    private def formatCell(name: String)(cell: Pair[Node, TextDescriptor]) = {
        val node = cell._1
        val text = cell._2
        <td>{
                if (!text.edit) {
                    formatContent(node)
                } else {
                    val inputName = "edit-" + text.name + name
                    <input type="text" name={inputName} value={formatEditable(node)}/>
                }
        }</td>  
    }
}
