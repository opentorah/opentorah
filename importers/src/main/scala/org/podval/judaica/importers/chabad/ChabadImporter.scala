/*
 *  Copyright 2011 dub.
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

import scala.xml.Node

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
        val index = TagSoupXmlLoader.get().loadFile(file)

        val path = getFileBase(file)

        // TODO name
        <div type="book">{
            parseIndex(index).zipWithIndex.map {
                case (name, chapterNumberFrom0) =>
                    <div type="chapter" n={(chapterNumberFrom0+1).toString()}>{
                        parseChapter(path + name);
                    }</div>
            }
        }</div>
    }


    private def parseIndex(index: Node): Seq[String] =
        index \\ "table" \\ "@href" map(_.text) map(getFileName)


    private def parseChapter(path: String) =
        (loadFile(path) \\ "body" \ "_")
        .filter { e => Set("bodytext", "hagoho").contains((e \ "@class").text) }
        .filter { case <p/> => false case _ => true }


    private def getFileBase(file: File): String = getFileBase(file.getAbsolutePath)


    private def getFileBase(path: String): String = path.substring(0, path.lastIndexOf("/")+1)


    private def getFileName(path: String): String = path.substring(path.lastIndexOf("/")+1)


    private def loadFile(path: String): Node =
        TagSoupXmlLoader.get().loadFile(new File(path))


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
