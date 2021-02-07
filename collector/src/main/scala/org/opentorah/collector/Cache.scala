package org.opentorah.collector

import com.github.benmanes.caffeine.cache.{Caffeine, RemovalCause}
import org.slf4j.{Logger, LoggerFactory}
import java.net.URL

object Cache {

  private val log: Logger = LoggerFactory.getLogger(classOf[Cache.type])

  private val cache: com.github.benmanes.caffeine.cache.Cache[URL, AnyRef] = Caffeine.newBuilder
    .softValues()
    .removalListener((key: URL, value: AnyRef, cause: RemovalCause) => log.info(s"EVICTED $key because $cause"))
    //.expireAfterWrite(10, TimeUnit.MINUTES)
    //.maximumSize(100)
    //.weakValues()
    //.weakKeys()
    //.maximumWeight()
    //.recordStats()
    //.writer()
    .build[URL, AnyRef]

  // TODO pass in parser from the file at the URL
  // TODO support auto-refresh (if the file changed after it was cached )
  //   Look up and compute an entry if absent, or null if not computable
  //   graph = cache.get(key, (k: Nothing) => createExpensiveGraph(key))
  def get[T <: AnyRef](url: URL, load: URL => T): T =
    Option[AnyRef](cache.getIfPresent(url)).map(_.asInstanceOf[T]).getOrElse {
      val result = load(url)
      cache.put(url, result)
      //log.info(s"CACHED  $url of type ${result.getClass.getSimpleName}")
      result
    }
}
