/*
 *  Copyright 2011 Leonid Dubinsky <dub@podval.org>.
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

import scala.xml.{Node, Elem}

/*
 * During Shavues 5771, I discussed the Chumash Taich interface with Zev.
 * He thinks that it is not important to present the text for Taich input in
 * non-natural structure like weekly portions; so we can save on the reverse transform.
 * 
 * Zev listed interesting features of Taich:
 * 
 * Sometimes, more than one word need to be translated (=> attribute "span" in internal representation?).
 * Sometimes, there are more than one translation (or one is elucidation of the other).
 * Sometimes, translation has an explanation attached (in Hirshl's original - parens and quotes are used).
 * It is desirable to be prompted with translation of the given word that were already entered - in an overridable way.
 * 
 * URL encoding request:
 * SEL: selection part - sequence of bindings between selectors and values
 * separator *structure*
 * STR: structure selection part - sequence of selectors
 * suffix: *content* or *selectors*
 */

final class Viewer
//(
//    format: TextFormat,
//    selectionDivType: String,
//    texts: Seq[Text])
{
//    def get(postUrl: String): Seq[Node] = {
//        format(postUrl, merge(texts.head, texts.tail))
//    }
//
//
//    def put(form : Map[String, List[String]]): String = {
//        unique(form).toString
//    }


//    // @todo handle multiple "others"
//    private def merge(main: Text, others: Seq[Text]): Seq[Node] = {
//        merge(main.getXml(), others.head.getXml(), format.divTypes)
//    }
//
//
//    private def merge(main: Seq[Node], others: Seq[Node], divTypes: Seq[String]): Seq[Node] = {
//        if (divTypes.isEmpty) {
//            zipOrElse(main, others).map(
//                {case (child, otherChild) => <merge>{child}{otherChild}</merge>})
//        } else {
//            val divType = divTypes.head
//            zipOrElse(main, others.filter(Selector.isDivType(divType))).map(
//                {case (child, otherChild) => merge(child, otherChild, divType, divTypes.tail)})
//        }
//    }
//
//
//    private def merge(child: Node, otherChild: Node, divType: String, remainingDivTypes: Seq[String]): Node = {
//        if (!Selector.isDivType(divType)(child)) child else {
//            if (Selector.getName(child) != Selector.getName(otherChild)) {
//                throw new IllegalArgumentException("Different names!")
//            }
//            
//            Elem(
//                child.namespace,
//                child.label,
//                child.attributes,
//                child.scope,
//                merge(child.child, otherChild.child, remainingDivTypes): _*
//            )
//        }
//    }


    private def zipOrElse[A,B](left: Seq[A], right: Seq[B]): Seq[(A,B)] = {
        if (left.size != right.size) throw new IllegalArgumentException("Wrong lengths")
 
        left.zip(right)
    }


//    private def format(postUrl: String, nodes: Seq[Node]): Seq[Node] = {
//        <form action={postUrl} method="post">
//            {format(nodes, Seq())}
//            <input type="submit" value="Submit"/>
//        </form>
//    }


//    private def format(nodes: Seq[Node], context: Seq[Node]): Seq[Node] = {
//        for (node <- nodes) yield {
//            val maybeSelector = Selector.maybeFromXml(node)
//
//            if (maybeSelector.isDefined) {
//                format.formatNonStructural(node)
//            } else {
//                format(maybeSelector.get, node, context)
//            }
//        }
//    }
//
//
//    private def format(selector: Selector, node: Node, context: Seq[Node]): Node = {
//        val newContext = context :+ node
//
//        <div class={selector.what}>
//            {selector.toNameSpan}
//            {
//                if (selector.what != format.getMergeDivType()) {
//                    format(node.child, newContext)
//                } else {
//                    formatTable(node, newContext)
//                }
//            }
//        </div>
//    }
//
//
//    private def formatTable(node: Node, context: Seq[Node]) = {
//        val divName = nameFromContext(context)
//
//        <table>
//            <tr>{
//                texts.map(text => <td>{new Selector("text", text.name).toNameSpan}</td>)
//            }</tr>
//            {
//                node.child.zipWithIndex.map({case (row,numRow) =>
//                    if (row.label != "merge") {
//                        throw new IllegalArgumentException("Not a 'merge'!")
//                    }
//
//                    val content = row.child
//
//                    if (content.size != texts.size) {
//                        throw new IllegalArgumentException("Wrong length")
//                    }
//
//                    val rowName = divName + "-" + (numRow+1)
//
//                    <tr>{
//                        content.zip(texts).map({case (node, text) =>
//                            <td>{
//                                if (!text.isEdit) {
//                                    format.formatContent(node)
//                                } else {
//                                    val inputName = "edit-" + text.name + "-" + rowName
//                                    <input type="text" name={inputName} value={format.formatEditable(node)}/>
//                                }
//                            }</td>  
//                        })
//                    }</tr>
//                })
//            }
//        </table>
//    }
//
//
//    private def nameFromContext(context: Seq[Node]) = {
//        Selector.toName(context.map(Selector.fromXml).dropWhile(_.what != selectionDivType))
//    }


    private def unique(form: Map[String, List[String]]): Map[String, String] = {
        form.map({ case (name, values) =>
            if (values.size > 1) {
                
            }

            name -> values(0)
        })
    }
}
