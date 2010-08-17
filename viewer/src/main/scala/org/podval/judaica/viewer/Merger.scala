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


final class Merger(divTypes: Seq[String]) {

    // @todo handle multiple "others"
    def merge(main: Text, others: Seq[Text]): Seq[Node] = {
        merge(main.getXml(), others.head.getXml(), divTypes)
    }


    private def merge(main: Seq[Node], others: Seq[Node], divTypes: Seq[String]): Seq[Node] =
        divTypes match {
        case List(divType, remainingDivTypes) =>
            for (Pair(child, otherChild) <- zipOrElse(main, others.filter(Selector.isDivType(divType)))) yield
                mergeStructurally(child, otherChild, divType, remainingDivTypes)
        case Nil =>
            for (Pair(child, otherChild) <- zipOrElse(main, others)) yield
                <merge>{child}{otherChild}</merge>
        }


    private def mergeStructurally(child: Node, otherChild: Node, divType: String, divTypes: Seq[String]) = {
        if (!Selector.isDivType(divType)(child)) child else {
            if (Selector.getName(child) != Selector.getName(otherChild)) {
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


    private def zipOrElse[A,B](left: Seq[A], right: Seq[B]): Seq[(A,B)] = {
        if (left.size != right.size) throw new IllegalArgumentException("Wrong lengths")
 
        left.zip(right)
    }
}
