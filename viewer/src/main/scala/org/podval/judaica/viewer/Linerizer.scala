package org.podval.judaica.viewer

import java.io.PrintWriter

object Linerizer {

  def main(args: Array[String]): Unit = {
    val out: PrintWriter = new PrintWriter("/tmp/linear.txt")
    val work: Content = Selection("Tanach", "Jerusalem")
//      .selectPath("book/Genesis/chapter/1/verse/2")
    .selectPath("book/Genesis/chapter/1")
      .asStructure.content(None)
    print(work.asInstanceOf[DivContent], out)
    out.close()
  }

  private def print(work: DivContent, out: PrintWriter): Unit = {
    assert(work.sort == "work" && work.n.contains("Tanach"))
    val book: DivContent = work.children.head.asInstanceOf[DivContent]
    assert(book.sort == "book")
    out.println(book.head.get)
    book.children.foreach(chapter => printChapter(chapter, out))
  }

  private def printChapter(raw: Content, out: PrintWriter): Unit = {
    val chapter = raw.asInstanceOf[DivContent]
    assert(chapter.sort == "chapter")
    out.println(chapter.head.get)
    chapter.children.map(_.asInstanceOf[DivContent]).foreach { content =>
      if (content.sort == "paragraph")
        printParagraph(content, out)
      else
        printVerse(content, out)
    }
  }

  private def printParagraph(paragraph: DivContent, ouit: PrintWriter): Unit = {
    // TODO
  }

  private def printVerse(raw: Content, out: PrintWriter): Unit = {
    val verse = raw.asInstanceOf[DivContent]
    assert(verse.sort == "verse")
    out.println(verse.head.get)
    val words = verse.children.filter(_.isInstanceOf[DivContent]).map(_.asInstanceOf[DivContent])
    words.init.foreach(word => printWord(word, sofPasuk = false, out))
    printWord(words.last, sofPasuk = true, out)
//    Xml.print(Content.toXmlNode(verse), System.out)
  }

  private def printWord(word: DivContent, sofPasuk: Boolean, out: PrintWriter): Unit = {
    assert(word.sort == "word")
    // In Jerusalem edition, Makafs are in the word text...
    //val hasMakaf: Boolean = word.attributes.get("makaf").isDefined
    val text: String = word.children.head.asInstanceOf[TextContent].text
    val result: String = text ++ (if (sofPasuk) HebrewLanguage.SOF_PASUQ else "")
    out.println(result)
  }
}
