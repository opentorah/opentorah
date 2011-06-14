/*
 *  Copyright 2011 Leonid Dubinsky <dub@podval.org>.
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  under the License.
 */

package org.podval.judaica.importers
package chabad

import scala.xml.{Node, Utility}

import java.io.File


object ChabadImporter {

    def main(args: Array[String]) {
        val importer = new ChabadImporter(
            "/mnt/data/www.chabadlibrary.org/books2/",
            "/var/www/sites/app.judaica/site/raw/")

        importer.importBook("adhaz/tanya/1/index", "Tanya1");
    }
}


class ChabadImporter(inputDirectory: String, outputDirectory: String) extends Importer(inputDirectory, outputDirectory) {

    def getInputExtension() : String = "htm"


    def getStylesheet(): String = "chabad.css"


    def parseBook(file: File): Node = {
        val index = load(file)

        val directory = file.getParentFile

        // TODO name
        <div type="book">{
            parseIndex(index).zipWithIndex.map {
                case (name, chapterNumberFrom0) =>
                    <div type="chapter" n={(chapterNumberFrom0+1).toString()}>{
                        parseChapter(new File(directory, name));
                    }</div>
            }
        }</div>
    }


    private def parseIndex(index: Node): Seq[String] =
        index \\ "table" \\ "@href" map(_.text) map(getFileName)


    private def parseChapter(file: File) = {
        val raw: Seq[Node] =
            // get all elements of the <body>
            (load(file) \\ "body" \ "_")
            // filter only elements with content
            .filter { e => Set("bodytext", "hagoho").contains((e \ "@class").text) }
            // remove empty <p>s
            .filter { case <p/> => false case _ => true }
            // remove "hagoho" title
            .filter { case <div><b>{_*}</b></div> => false case _ => true}

        split[Node](raw, { case <p><span>{_*}</span>{_*}</p> => false case _ => true})
        .map(p => <p>{p map parseParagraphChunk}</p>)
    }


    private def parseParagraphChunk(n: Node) = n match {
        case <div>{t @ _*}</div> => <note>{t}</note>
        case <p>{t @ _*}</p> => t map parseTextChunk
    }


    private def parseTextChunk(n: Node) = n match {
        case <span>{t @ _*}</span> => t
        case t => t
    }


    def split[A](s: Seq[A], p: (A) => Boolean): Seq[Seq[A]] =
      if (s.isEmpty) Nil else split1(s.head, s.tail, p)
// For some reason, case-based approach results in MatchError!
//      s match {
//        case Nil => Nil
//        case h :: xs => split1(h, xs, p)
//    }


    def split1[A](h: A, s: Seq[A], p: (A) => Boolean) = s span p match { // lift ~
        case (ns, xs) => Seq(Seq(h) ++ ns) ++ split(xs, p)
    }


    private def load(file: File): Node = Utility.trim(TagSoupXmlLoader.get().loadFile(file))


    private def getFileName(path: String): String = path.substring(path.lastIndexOf("/")+1)


//    <!-- select interesting elements -->
//    <!-- @todo join paragraphs split by a note -->
//    <!-- @todo deal with parentheses -->
//    <!-- @todo introduce corrections (e.g., in chapter 51 of Tanya, the kind of the note is wrong) -->
//    <xsl:template match="/">
//        <xsl:variable name="selected">
//            <xsl:apply-templates select="./html:html/html:frameset/html:noframes/html:body/*" mode="select"/>
//        </xsl:variable>
//
//        <xsl:variable name="grouped">
//            <xsl:for-each-group select="$selected/*" group-adjacent="if (self::note) then 0 else position()">
//                <xsl:choose>
//                    <xsl:when test="current-grouping-key() eq 0">
//                        <note type="side">
//                            <xsl:copy-of select="for $n in current-group() return $n/node()"/>
//                        </note>
//                    </xsl:when>
//                    <xsl:otherwise>
//                        <xsl:copy-of select="current-group()"/>
//                    </xsl:otherwise>
//                </xsl:choose>
//            </xsl:for-each-group>
//        </xsl:variable>
//
//        <xsl:variable name="lined">
//            <xsl:apply-templates select="$grouped" mode="lines"/>
//        </xsl:variable>
//
//        <xsl:copy-of select="$lined"/>
//    </xsl:template>
//
//
//    <!--                          -->
//    <!-- skip uninteresting stuff -->
//    <!--                          -->
//
//
//    <!-- main text -->
//    <xsl:template match="html:p[@class eq 'bodytext']" mode="select">
//        <xsl:variable name="content">
//            <xsl:apply-templates mode="main-text"/>
//        </xsl:variable>
//
//        <!-- I want to skip empty "p" elements, and this is the only test that I know that works... -->
//        <xsl:if test="string($content) ne ''">
//            <p>
//                <xsl:copy-of select="$content"/>
//            </p>
//        </xsl:if>
//    </xsl:template>
//
//
//    <!-- side note -->
//    <xsl:template match="html:div[@class eq 'hagoho']" mode="select">
//        <note>
//            <xsl:apply-templates mode="side-note"/>
//        </note>
//    </xsl:template>
//
//
//    <!-- skip everything else -->
//    <xsl:template match="@*|*" mode="select"/>
//
//
//    <!-- peel off the 'span' (in the beginning of a paragraph) -->
//    <xsl:template match="html:span[@class eq 'cup']" mode="main-text">
//        <xsl:apply-templates/>
//    </xsl:template>
//
//
//    <!-- Editorial note -->
//    <xsl:template match="html:b" mode="main-text">
//        <note type="editorial">
//            <xsl:apply-templates/>
//        </note>
//    </xsl:template>
//
//
//    <!-- In the beginning of a side-note (only?) there may be a title -->
//    <xsl:template match="html:b" mode="side-note">
//        <p type="title">
//            <xsl:apply-templates/>
//        </p>
//    </xsl:template>
//
//
//    <!--                             -->
//    <!-- break paragraphs into lines -->
//    <!--                             -->
//
//
//    <xsl:template match="@*|*" mode="lines">
//        <xsl:copy>
//            <xsl:apply-templates select="@*|node()" mode="#current"/>
//        </xsl:copy>
//    </xsl:template>
//
//
//    <xsl:template match="text()" mode="lines">
//        <xsl:variable name="no-colon" select="replace(., ':', '')"/>
//        <xsl:analyze-string select="$no-colon" regex="\.">
//            <xsl:matching-substring>
//                <dot/>
//            </xsl:matching-substring>
//            <xsl:non-matching-substring>
//                <xsl:copy-of select="."/>
//            </xsl:non-matching-substring>
//        </xsl:analyze-string>
//    </xsl:template>
}
