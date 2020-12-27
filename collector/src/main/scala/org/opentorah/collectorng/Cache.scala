package org.opentorah.collectorng

import com.github.benmanes.caffeine.cache.Caffeine
import java.net.URL

// TODO make less global / ZIOify ;)
object Cache {

  private val cache: com.github.benmanes.caffeine.cache.Cache[URL, AnyRef] = Caffeine.newBuilder
    //.expireAfterWrite(10, TimeUnit.MINUTES)
    //.maximumSize(100)
    .weakKeys()
    .softValues() //.weakValues()
    //.maximumWeight()
    //.recordStats()
    //.writer()
    .build[URL, AnyRef]

  // TODO pass in parser from the file at the URL
  //   Look up and compute an entry if absent, or null if not computable
  //   graph = cache.get(key, (k: Nothing) => createExpensiveGraph(key))
  def get[T <: AnyRef](url: URL, load: URL => T): T =
    Option[AnyRef](cache.getIfPresent(url)).map(_.asInstanceOf[T]).getOrElse {
      val result = load(url)
      cache.put(url, result)
//      println(s"cached $url of type ${result.getClass.getSimpleName}")
      result
    }
}
