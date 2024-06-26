package org.opentorah.collector

import org.opentorah.site.Site
import org.opentorah.store.{Path, Store}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Caching, FromUrl, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

final class CollectorTest extends AnyFlatSpec, Matchers:

  // Collector
  val localStorePath: String = "/home/dub/OpenTorah/alter-rebbe.org/"
  val isLocal: Boolean = File(localStorePath).exists()
  val siteUrl: String = if isLocal then s"file:$localStorePath" else s"http://${Collector.bucketName}/"
  val collector: Collector = Effects.unsafeRun(Collector.readSite(siteUrl))
  collector.caching.logEnabled = false

  "Collector smoke tests" should "work" in {
//    val site: ScalaXml.Element = Effects.unsafeRun(From.url(Files.file2url(File(localStorePath + "site.xml")), ScalaXml).load).asInstanceOf[ScalaXml.Element]
//    println(site)

    def getResponse(pathString: String): Either[Throwable, Site.Response] =
      Effects.unsafeRun(collector.getResponse(pathString).either)
    def getContent(pathString: String): String = getResponse(pathString).getOrElse(fail()).content
    def getError(pathString: String): String = getResponse(pathString).left.getOrElse(fail()).getMessage

    def getStoreUrl(pathString: String): String =
      val path: Path = Effects.unsafeRun(collector.toTask(collector.resolveUrl(pathString))).get
      path.last.asInstanceOf[FromUrl.With].fromUrl.url.toString

    getStoreUrl("/rgada") should endWith("archive/rgada/category/VII/inventory/2/case/3140.xml")
    getStoreUrl("/archive/rgada/") should endWith("/archive/rgada.xml")

    getContent("/") should include("Дела")
    getContent("/collections") should include("Архивы")
    getContent("/note/about") should include("Цель настоящего сайта: ")
    getContent("/note/help") should include("навигационная полоса содержит:")

    // This one verifies that the name lists are set up properly, even when the list of lists has not
    // been accessed yet:
    // TODO !!! GithHub Actions build fails with out-of-memory - probably because of the references...
//    getContent("/names/jews/alter-rebbe.html") should include("основатель направления Хабад")
//    getContent("/names") should include("Жиды (они же Евреи)")
//    getContent("/names/jews") should include("Жиды (они же Евреи)")
//    getContent("/names/jews/alter-rebbe") should include("основатель направления Хабад")
//    getContent("/name") should include("Залман Борухович")
//    getContent("/name/alter-rebbe") should include("основатель направления Хабад")

    getContent("/dubnov") should include("Вмешательство")
    getContent("/dubnov/index") should include("Вмешательство")
    getContent("/dubnov/index.html") should include("Вмешательство")
    getContent("/rgada") should include("Швайгер; новые - Елена")
    getContent("/rgada/029") should include("о сѣктѣ каролиновъ")
    getError  ("/rgada/029/index") should include("get an index")
    getContent("/rgada/029.html") should include("о сѣктѣ каролиновъ")
    getContent("/rgada/029.xml") should include("о сѣктѣ каролиновъ")
    getContent("/rgada/document/029") should include("о сѣктѣ каролиновъ")
    getContent("/rgada/document/029.html") should include("Черновой вариант")
    getContent("/rgada/document/029.xml") should include("Черновой вариант")
    getContent("/rgada/facsimile/029") should include("Черновой вариант")
    getContent("/rgada/facsimile/029.html") should include("Черновой вариант")
    getError  ("/rgada/facsimile/029.xml") should include("non-HTML content")

    getContent("/archive/rgada/category/VII/inventory/2/case/3140/document/001") should include("100 рублей")
  }

  "Store.getPath()" should "work" in {
    val reportPaths: Seq[Path] = Parser.unsafeRun(caching = new Caching.Simple, parser = Reports.getPaths(
      include = _.isInstanceOf[Report[?]],
      stop = _.isInstanceOf[Report[?]]
    ))
    // TODO assert something meaningful: println(reportPaths.map(Path.structureNames(_).mkString("/")).mkString("\n"))
  }
