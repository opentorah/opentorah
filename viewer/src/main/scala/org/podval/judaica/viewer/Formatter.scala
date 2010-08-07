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


object Formatter {

    def format(nodes: Seq[Node], what: String): Seq[Node] =
      format(nodes, what, "")


    def format(nodes: Seq[Node], what: String, name: String): Seq[Node] = {
        for (node <- nodes) yield {
            if (!Util.isDiv(node)) {
                formatNonStructural(node)
            } else {
                // name span

                val newName = (if (name.isEmpty) name else name + "-") + Util.getName(node)

                <div class={Util.getType(node)}>{
                    if (Util.isDivType(node, what)) {
                        formatTable(node, newName)
                    } else {
                        format(node.child, what, newName)
                    }
                }
                </div>
            }
        }
    }


    private def formatTable(node: Node, name: String) = {
       <table>
           {node.child.map(formatRow)}
       </table>
   }


    private def formatRow(node: Node): Seq[Node] = {
        if (node.label != "merge") {
            throw new IllegalArgumentException("Not a 'merge'!")
        }

        <tr>
          {node.child.map(formatCell)}
        </tr>
    }


    private def formatCell(node: Node) = {
        // @todo handle app...
        if (node.label != "word") {
            throw new IllegalArgumentException("Unknown merged tag " + node.label)
        }

        <td>{node.text}</td>  
    }


    private def formatNonStructural(node: Node) = {
        null
    }  
}
