package org.opentorah.collector

import com.github.benmanes.caffeine.cache.{Caffeine, RemovalCause}
import org.slf4j.{Logger, LoggerFactory}
import java.net.URL
import java.time.Duration

object Cache {

  var enabled: Boolean = true
  var logEnabled: Boolean = true

  private val log: Logger = LoggerFactory.getLogger(classOf[Cache.type])

  private val cache: com.github.benmanes.caffeine.cache.Cache[URL, AnyRef] = Caffeine.newBuilder
    .softValues()
    .expireAfterAccess(Duration.ofMinutes(10))
    .removalListener((key: URL, value: AnyRef, cause: RemovalCause) => log.info(s"EVICTED ($cause) $key"))
    .build[URL, AnyRef]

  def get[T <: AnyRef](url: URL, load: URL => T): T =
    if (!enabled) load(url) else Option[AnyRef](cache.getIfPresent(url)).map(_.asInstanceOf[T]).getOrElse {
      val result: T = load(url)
      cache.put(url, result)
      if (logEnabled) log.info(s"CACHED ${result.getClass.getSimpleName} $url")
      result
    }
}
