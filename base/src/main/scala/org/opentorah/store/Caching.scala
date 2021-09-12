package org.opentorah.store

import com.github.benmanes.caffeine.cache.{Cache, Caffeine, RemovalCause}
import org.opentorah.util.Effects
import org.opentorah.xml.Context
import org.slf4j.{Logger, LoggerFactory}
import zio.{Has, ZIO, ZLayer}
import java.net.URL
import java.time.Duration

trait Caching:
  def getCached[T <: AnyRef](url: URL, load: URL => Caching.Parser[T]): Caching.Parser[T]

object Caching:

  type Parser[+A] = ZIO[Has[Caching] & Has[Context], Effects.Error, A]

  def getCached[T <: AnyRef](url: URL, load: URL => Parser[T]): Parser[T] =
    ZIO.accessM[Has[Caching] & Has[Context]](_.get.getCached[T](url, load))

  def provide[A](caching: Caching, parser: Parser[A]): org.opentorah.xml.Parser[A] =
    parser.provideSomeLayer[Has[Context]](ZLayer.succeed(caching))

  private val log: Logger = LoggerFactory.getLogger(classOf[Caching.type])

  final class Simple extends Caching:
    private val cache: Cache[URL, AnyRef] = Caffeine.newBuilder
      .softValues()
      .expireAfterAccess(Duration.ofMinutes(10))
      .removalListener((key: URL, value: AnyRef, cause: RemovalCause) => log.info(s"EVICTED ($cause) $key"))
      .build[URL, AnyRef]

    var logEnabled: Boolean = true

    override def getCached[T <: AnyRef](url: URL, load: URL => Parser[T]): Parser[T] =
      Option[AnyRef](cache.getIfPresent(url)).map(_.asInstanceOf[T]).map(ZIO.succeed(_)).getOrElse(
        load(url).map(result =>
          cache.put(url, result)
          if logEnabled then log.info(s"CACHED ${result.getClass.getSimpleName} $url")
          result
        )
      )
