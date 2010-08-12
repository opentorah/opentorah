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

import scala.xml.{Node, Elem}


object Merger {

    // @todo handle multiple "others"
    def merge(main: Seq[Node], others: Seq[Node], divTypes: Seq[String]) = {
        if (!divTypes.isEmpty) {
            mergeStructurally(main, others, divTypes)
        } else {
            mergePositionally(main, others)
        }
    }


    private def mergeStructurally(main: Seq[Node], others: Seq[Node], divTypes: Seq[String]): Seq[Node] = {
        val divType = divTypes.head

        val othersIterator = others.filter(Util.isDivType(_, divType)).iterator

        for (child <- main) yield {
            if (!Util.isDivType(child, divType)) child else {
                val otherChild = doGetNext(othersIterator)

                if (Util.getName(child) != Util.getName(otherChild)) {
                    throw new IllegalArgumentException("Different names!")
                }

                Elem(
                    child.namespace,
                    child.label,
                    child.attributes,
                    child.scope,
                    merge(child.child, otherChild.child, divTypes.tail): _*
                )
            }
        }
    }


    private def mergePositionally(main: Seq[Node], others: Seq[Node]): Seq[Node] = {
        val othersIterator = others.iterator

        for (child <- main) yield {
            val otherChild = doGetNext(othersIterator)

            <merge>{child}{otherChild}</merge>
        }
    }


    private def doGetNext(iterator: Iterator[Node]) = {
        if (!iterator.hasNext) {
            throw new IllegalArgumentException("Premature end of the othersIterator")
        }

        iterator.next
    }
}
