package org.opentorah.xml

import com.github.benmanes.caffeine.cache.{Cache, Caffeine, RemovalCause}
import org.opentorah.util.Effects
import org.slf4j.{Logger, LoggerFactory}
import zio.{ZIO, ZLayer}
import java.net.URL
import java.time.Duration

trait Caching:
  def getCachedByUrl[T <: AnyRef](url: URL, load: URL => Caching.Parser[T]): Caching.Parser[T]

  def getCached[T <: AnyRef](key: AnyRef, load: => Caching.Parser[T]): Caching.Parser[T]

object Caching:

  type Parser[+A] = ZIO[Caching & Parsing, Effects.Error, A]

  def getCachedByUrl[T <: AnyRef](url: URL, load: URL => Parser[T]): Parser[T] =
    ZIO.environmentWithZIO[Caching](_.get.getCachedByUrl[T](url, load))

  def getCached[T <: AnyRef](key: AnyRef, load: => Parser[T]): Parser[T] =
    ZIO.environmentWithZIO[Caching](_.get.getCached[T](key, load))

  def unsafeRun[T](caching: Caching, parser: Parser[T]): T =
    org.opentorah.xml.Parser.unsafeRun(provide(caching, parser))

  def provide[T](caching: Caching, parser: Parser[T]): org.opentorah.xml.Parser[T] =
    parser.provideSomeLayer[Parsing](ZLayer.succeed(caching))

  private val log: Logger = LoggerFactory.getLogger(classOf[Caching.type])

  // TODO introduce Caching.None.

  final class Simple extends Caching:
    private val cache: Cache[AnyRef, AnyRef] = Caffeine.newBuilder
      .softValues()
      .expireAfterAccess(Duration.ofMinutes(10))
      .removalListener((key: AnyRef, value: AnyRef, cause: RemovalCause) => log.info(s"EVICTED ($cause) $key"))
      .build[AnyRef, AnyRef]

    var logEnabled: Boolean = true

    override def getCachedByUrl[T <: AnyRef](url: URL, load: URL => Parser[T]): Parser[T] =
      getCached[T](url, load(url))

    def getCached[T <: AnyRef](key: AnyRef, load: => Caching.Parser[T]): Caching.Parser[T] =
      Option[AnyRef](cache.getIfPresent(key))
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
