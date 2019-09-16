package org.podval.judaica.importers
package chabad

import scala.xml.{Elem, Node, Utility}
import java.io.File
import org.podval.judaica.viewer.{DivContent, Content, Edition}

object ChabadImporter {
  def main(args: Array[String]): Unit = {
    new ChabadImporter() {
      override def workName = "Tanya"
    }.importWork("/home/dub/code/judaica/imports/Chabad/") // TODO
  }
}


abstract class ChabadImporter extends Importer {
  protected final override def editionName: String = "Chabad"

  protected final override def getInputExtension: String = "htm"

  protected final override def books: Map[String, String] = Map(
    "Tanya/1/index" -> "Tanya/1"
  )

  def parseBook(file: File, outputName: String): DivContent = {
    val index = load(file)

    val directory = file.getParentFile

    // TODO
    null
    //        // TODO name
    //        <div type="book">{
    //            parseIndex(index).zipWithIndex.map {
    //                case (name, chapterNumberFrom0) =>
    //                    <div type="chapter" n={(chapterNumberFrom0+1).toString()}>{
    //                        parseChapter(new File(directory, name));
    //                    }</div>
    //            }
    //        }</div>
  }


//    private def parseIndex(index: Node): Seq[String] =
//        index \\ "table" \\ "@href" map(_.text) map(getFileName)
//
//
//    private def parseChapter(file: File) = {
//        val raw: Seq[Node] =
//            // get all elements of the <body>
//            (load(file) \\ "body" \ "_")
//            // filter only elements with content
//            .filter { e => Set("bodytext", "hagoho").contains((e \ "@class").text) }
//            // remove empty <p>s
//            .filter { case <p/> => false case _ => true }
//            // remove "hagoho" title
//            .filter { case <div><b>{_*}</b></div> => false case _ => true}
//
//        split[Node](raw, { case <p><span>{_*}</span>{_*}</p> => false case _ => true})
//        .map(p => <p>{p map parseParagraphChunk}</p>)
//    }
//
//
//    private def parseParagraphChunk(n: Node) = n match {
//        case <div>{t @ _*}</div> => <note>{t}</note>
//        case <p>{t @ _*}</p> => t map parseTextChunk
//    }
//
//
//    private def parseTextChunk(n: Node) = n match {
//        case <span>{t @ _*}</span> => t
//        case t => t
//    }
//
//
//    def split[A](s: Seq[A], p: (A) => Boolean): Seq[Seq[A]] =
//      if (s.isEmpty) Nil else split1(s.head, s.tail, p)
//// For some reason, case-based approach results in MatchError!
////      s match {
////        case Nil => Nil
////        case h :: xs => split1(h, xs, p)
////    }
//
//
//    def split1[A](h: A, s: Seq[A], p: (A) => Boolean) = s span p match { // lift ~
//        case (ns, xs) => Seq(Seq(h) ++ ns) ++ split(xs, p)
//    }


  private def load(file: File): Node = Utility.trim(TagSoupXmlLoader.get().loadFile(file))


//    private def getFileName(path: String): String = path.substring(path.lastIndexOf("/")+1)
//
//
////    <!-- select interesting elements -->
////    <!-- @todo join paragraphs split by a note -->
////    <!-- @todo deal with parentheses -->
////    <!-- @todo introduce corrections (e.g., in chapter 51 of Tanya, the kind of the note is wrong) -->
////
////    <!-- peel off the 'span' (in the beginning of a paragraph) -->
////
////    <!-- Editorial note -->
////    <xsl:template match="html:b" mode="main-text">
////    <!-- In the beginning of a side-note (only?) there may be a title -->
////    <!-- break paragraphs into lines -->
}
