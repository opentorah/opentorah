package org.opentorah.xml

import com.github.benmanes.caffeine.cache.{Cache, Caffeine, RemovalCause}
import org.slf4j.{Logger, LoggerFactory}
import zio.ZIO
import java.net.URL
import java.time.Duration

trait Caching:
  final def getCachedByUrl[T <: AnyRef](url: URL, load: URL => Parser[T]): Parser[T] = getCached[T](url, load(url))

  def getCached[T <: AnyRef](key: AnyRef, load: => Parser[T]): Parser[T]

object Caching:
  private val log: Logger = LoggerFactory.getLogger(classOf[Caching.type])

  object Zero extends Caching:
    def getCached[T <: AnyRef](key: AnyRef, load: => Parser[T]): Parser[T] = load

  final class Simple extends Caching:
    private val cache: Cache[AnyRef, AnyRef] = Caffeine.newBuilder
      .softValues()
      .expireAfterAccess(Duration.ofMinutes(10))
      .removalListener((key: AnyRef, _: AnyRef, cause: RemovalCause) => log.info(s"EVICTED ($cause) $key"))
      .build[AnyRef, AnyRef]

    var logEnabled: Boolean = true

    def getCached[T <: AnyRef](key: AnyRef, load: => Parser[T]): Parser[T] = Option[AnyRef](cache.getIfPresent(key))
      .map(_.asInstanceOf[T])
      .map(ZIO.succeed)
      .getOrElse(
        for
          _ <- ZIO.succeed(if logEnabled then log.info(s"LOADING $key"))
          result <- load
          _ <- ZIO.succeed(cache.put(key, result))
          _ <- ZIO.succeed(if logEnabled then log.info(s"CACHED  ${result.getClass.getSimpleName} $key"))
        yield result
      )
