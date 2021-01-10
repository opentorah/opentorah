package org.opentorah.collectorng

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class SiteTest extends AnyFlatSpec with Matchers {

  "Site" should "work" ignore {
    //val old = new org.opentorah.collector.Site(new java.io.File("/home/dub/OpenTorah/alter-rebbe.org").toURI.toURL)
    val site: Site = Site.read("/home/dub/OpenTorah/alter-rebbe.org")
    site.writeLists()
    site.writeStaticFiles()

//    site.by.stores.head.names.name shouldBe "книги"
//
//    site.resolve("/archive/книги").get
//    site.resolve("/archive/книги/book/Дубнов").get
//    site.resolve("/archive/LVIA/fund/378/division/bendrasis/inventory/1799/дело/178").get
//
//    val document = site.resolve("/archive/LVIA/fund/378/division/bendrasis/inventory/1799/дело/178/document/288").get
//    Store.toEnglishUrl(document) shouldBe "/archive/LVIA/fund/378/division/general/inventory/1799/case/178/document/288.html"
//    Store.toRussianUrl(document) shouldBe "/архив/LVIA/фонд/378/отдел/общий/опись/1799/дело/178/документ/288.html"
//
//    site.resolve("/archive/LVIA/fund/378/division/bendrasis/inventory/1799/дело/178/288").get
//
//    site.resolve("/rgia2213/002").get
//    site.resolve("/rgia2213/document/002").get
//    site.resolve("/rgia2213/facsimile/002.html").get
//
//    site.resolve("/note/about").get
//    site.resolve("/name/ALD").get

// TODO ->    Files.write(new File("/tmp/ci.html"), site.content(site.resolve("/rgia529").get))
    //    site.content(site.resolve("/archive/LVIA/fund/378/division/bendrasis/inventory/1799/дело/178/tei/288").get)
//    site.content(site.resolve("/rgia2213/002").get)
  }
}
