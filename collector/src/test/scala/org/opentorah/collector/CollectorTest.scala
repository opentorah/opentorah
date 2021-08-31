package org.opentorah.collector

import org.opentorah.site.Site
import org.opentorah.util.Effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

final class CollectorTest extends AnyFlatSpec with Matchers {

  "Collector smoke tests" should "work" in {
    val localStorePath: String = "/home/dub/OpenTorah/alter-rebbe.org/"
    val isLocal: Boolean = new File(localStorePath).exists()
    val siteUrl: String = if (isLocal) s"file:$localStorePath" else s"http://${CollectorService.bucketName}/"
    val collector: Collector = Effects.unsafeRun(CollectorService.readSite(siteUrl))

    def resolve(pathString: String): Option[Site.Response] = Effects.unsafeRun(collector.resolveContent(pathString))
    def resolveContent(pathString: String): String = resolve(pathString).get.content

    resolveContent("/") should include("1841 год, дело III отделения на")
    resolveContent("/note/about") should include("Цель настоящего сайта: ")
    resolveContent("/note/help") should include("навигационная полоса содержит:")
    resolveContent("/collections") should include("Архивы")
    resolveContent("/names") should include("Жиды (они же Евреи)")
    resolveContent("/name/alter-rebbe") should include("основатель направления Хабад")
    resolveContent("/rgada") should include("новые - Елена Волк")
    resolveContent("/rgada/029") should include("о сѣктѣ каролиновъ")
  }
}
