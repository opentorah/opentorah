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

import org.podval.judaica.importers.AlefBeth

import scala.xml.{Elem, Text, Node, NodeBuffer, NodeSeq}


class Verse(var line: String, number: Int, metadata: NodeSeq) {

    private val PEI3 = AlefBeth.PEI + " " + AlefBeth.PEI + " " + AlefBeth.PEI


    private val SAMEH3 = AlefBeth.SAMEH + " " + AlefBeth.SAMEH + " " + AlefBeth.SAMEH


    private val HAZI =
        AlefBeth.HET +
        AlefBeth.TSADI +
        AlefBeth.YOD +
        " " +
        AlefBeth.HE +
        AlefBeth.SAMEH +
        AlefBeth.PEI +
        AlefBeth.RESH +
        " " +
        AlefBeth.BET +
        AlefBeth.PEI +
        AlefBeth.SAMEH +
        AlefBeth.VAV +
        AlefBeth.QOF +
        AlefBeth.YOD +
        AlefBeth.MEM_SOFIT


    private val HAZAK = AlefBeth.HET + AlefBeth.ZAYIN + AlefBeth.QOF


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

            result.appendAll(metadata)

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
        if (consume(AlefBeth.PEI)) {
            parsha = "open"
        } else
        if (consume(SAMEH3)) {
            parsha = "closed"
            big = true
        } else
        if (consume(AlefBeth.SAMEH)) {
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
        val makafIndex = line.indexOf(AlefBeth.MAQAF)

        def isFirst(one: Int, two: Int) = (one != -1) && ((two == -1) || (two > one))
        val isSpace = isFirst(spaceIndex, makafIndex)
        val isMakaf = isFirst(makafIndex, spaceIndex)

        val index = if (isSpace) spaceIndex else if (isMakaf) makafIndex else line.size
        val word = consumeToIndex(index)

        if (isSpace) {
            consume(" ")
        } else
        if (isMakaf) {
          consume(AlefBeth.MAQAF)
        }

        val isPasek = consume(AlefBeth.PASEQ)

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
