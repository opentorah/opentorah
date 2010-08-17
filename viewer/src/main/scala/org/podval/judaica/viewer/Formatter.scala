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

import scala.collection.mutable.Stack


class Formatter(
    format: TextFormat,
    selectionDivType: String,
    texts: Seq[Text],
    nodes: Seq[Node])
{
    val context = new Stack[Node]()


    def format(postUrl: String): Seq[Node] = {
        <form action={postUrl} method="post">
            {format(nodes)}
            <input type="submit" value="Submit"/>
        </form>
    }


    private def format(nodes: Seq[Node]): Seq[Node] = {
        for (node <- nodes) yield {
            Selector.maybeFromXml(node) match {
            case None => format.formatNonStructural(node)
            case Some(selector) => formatStructurally(selector, node)
            }
        }
    }


    private def formatStructurally(selector: Selector, node: Node) = {
        context.push(node)

        val result =
            <div class={selector.what}>
                {selector.toNameSpan}
                {
                    if (selector.what == format.getMergeDivType()) {
                        formatTable(node)
                    } else {
                        format(node.child)
                    }
                }
            </div>
        
        context.pop()

        result
    }


    private def formatTable(node: Node) = {
        val divName = nameFromContext()

        <table>
            <tr>{
                for (text <- texts) yield {
                    <td>{new Selector("text", text.name).toNameSpan}</td>
                }
            }</tr>
            {
                for (Pair(row,numRow) <- node.child.zipWithIndex) yield {
                    if (row.label != "merge") {
                        throw new IllegalArgumentException("Not a 'merge'!")
                    }

                    val content = row.child

                    if (content.size != texts.size) {
                        throw new IllegalArgumentException("Wrong length")
                    }

                    <tr>{
                        content.zip(texts).map(formatCell(divName + "-" + (numRow+1)))
                    }</tr>
                }
            }
        </table>
    }


    private def nameFromContext() = {
        Selector.toName(context.reverse.map(Selector.fromXml)
          .dropWhile(selector => selector.what != selectionDivType))
    }


    private def formatCell(name: String)(cell: Pair[Node, Text]) = {
        val node = cell._1
        val text = cell._2
        <td>{
            if (!text.isEdit) {
                format.formatContent(node)
            } else {
                val inputName = "edit-" + text.name + "-" + name
                <input type="text" name={inputName} value={format.formatEditable(node)}/>
            }
        }</td>  
    }
}
