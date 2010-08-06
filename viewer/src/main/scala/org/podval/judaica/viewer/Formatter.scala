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

    def format(nodes: Seq[Node], what: String): Seq[Node] = {
        for (node <- nodes) yield {
            if (Util.isDiv(node)) {
                <div class="{Util.getType(node)}">
                    {format(node.child, what)}
                </div>                
            } else if (Util.isDivType(node, what)) {
                format(node)
            } else {
                null
            }
        }
    }


    private def format(node: Node): Node = {
        <table>{
        for (row <- node.chil)
        }
        </table>
    }
}
