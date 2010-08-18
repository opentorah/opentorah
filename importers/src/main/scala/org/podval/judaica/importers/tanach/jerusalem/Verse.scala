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

package org.podval.judaica.importers.tanach.jerusalem

import scala.xml.{Elem, Text, Node, NodeBuffer}


class Verse(var line: String, number: Int) {

    private val SAMEH = "\u05E1"


    private val PEI = "\u05E4"


    private val PEI3 = PEI + " " + PEI + " " + PEI


    private val SAMEH3 = SAMEH + " " + SAMEH + " " + SAMEH


    private val HAZI = "חצי הספר בפסוקים" // @todo alefbeth


    private val HAZAK = "חזק" // @todo alefbeth


    private val MAKAF = "־" // @todo alefbeth


    private val PIPE = "|" // @todo alefbeth


    def parse(): Seq[Node] = {
        val result = new NodeBuffer()

        val parsha = processParsha()
        if (parsha != null) {
            result += parsha
        }

        consume(HAZI)
        consume(HAZAK)

        consumeBracketed()

        if (!line.isEmpty) {
            consumeToSpace()

            result +=
                <div type="verse" n={number.toString}>
                    {processWords()}
                </div>
        }

        result
    }


    private def processParsha(): Node = {
        var parsha: String = null
        var big = false

        if (consume(PEI3)) {
            parsha = "open"
            big = true
        } else
        if (consume(PEI)) {
            parsha = "open"
        } else
        if (consume(SAMEH3)) {
            parsha = "closed"
            big = true
        } else
        if (consume(SAMEH)) {
            parsha = "closed"
        }

        if (parsha == null) null else {
            <parsha type={parsha} big={booleanAttribute(big)}/>
        }
    }


    private def processWords(): Seq[Node] = {
        val result = new NodeBuffer()

        while (!line.isEmpty()) {
            val wordElement = processWord()

            val alternate = consumeBracketed()

            val element =
                if (alternate == null) {
                    wordElement
                } else {
                    <app>
                        <rdg type="write">
                            {wordElement}
                         </rdg>
                         <rdg type="read">
                             <word>{alternate}</word>
                         </rdg>
                    </app>
                }

            result += element
        }

        return result
    }


    private def processWord(): Elem = {
        val spaceIndex = line.indexOf(" ")
        val makafIndex = line.indexOf(MAKAF)

        def isFirst(one: Int, two: Int) = (one != -1) && ((two == -1) || (two > one))
        val isSpace = isFirst(spaceIndex, makafIndex)
        val isMakaf = isFirst(makafIndex, spaceIndex)

        val index = if (isSpace) spaceIndex else if (isMakaf) makafIndex else line.size
        val word = consumeToIndex(index)

        if (isSpace) {
            consume(" ")
        } else
        if (isMakaf) {
            consume(MAKAF)
        }

        val isPasek = consume(PIPE)

        <word makaf={booleanAttribute(isMakaf)} pasek={booleanAttribute(isPasek)}>{word}</word>
    }


    private def consumeToSpace(): String = {
        consumeToIndex(line.indexOf(" "))
    }


    private def consumeBracketed(): String = {
        if (line.startsWith("[")) {
            val index = line.indexOf("]")
            consumeToIndex(index+1).drop(1) // @todo tail()?
        } else {
            null
        }
    }


    private def consume(what: String): Boolean = {
        val result = line.startsWith(what)
        if (result) {
            consumeToIndex(what.length())
        }
        result
    }


    private def consumeToIndex(index: Int): String = {
        val result = line.take(index)
        // @todo get rid of the trim?
        line = line.drop(index).trim()
        result
    }


    private def booleanAttribute(value: Boolean) = {
        if (value) Some(Text("true")) else None
    }
}
