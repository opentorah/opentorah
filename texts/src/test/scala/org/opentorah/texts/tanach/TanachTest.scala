package org.opentorah.texts.tanach

import org.opentorah.store.{Caching, Store, Stores}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.Parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class TanachTest extends AnyFlatSpec with Matchers {
  // TODO is such setup needed only for tests - or should I abstract it?
  val caching: Caching.Simple = new Caching.Simple

  def resolve(path: String): Either[Effects.Error, Store.Path] =
    Effects.unsafeRun(Parser.toTask(Caching.provide(caching, Stores.resolve(Files.splitAndDecodeUrl(path), Tanach))))

  def doResolve(path: String): Store.Path = resolve(path).right.get
  def doResolveLast(path: String): Store = doResolve(path).last
  def checkName(path: String, name: String): Unit =
    doResolveLast(path).names.hasName(name) shouldBe true
  def checkChapterLength(path: String, length: Int): Unit =
    doResolveLast(path).asInstanceOf[Chapter].length shouldBe length
  def checkVerseNumber(path: String, number: Int): Unit =
    doResolveLast(path).asInstanceOf[VerseStore].number shouldBe number

  "Tanach" should "load" in {
    Chumash.Genesis.chapters.length(17) shouldBe 27
    Parsha.Vayikra.aliyot.spans(2).span.from.verse shouldBe 10
  }

  it should "contain /book/Genesis" in {
    checkName("/book/Genesis", "Бытие")
  }

  it should "contain /book/Бытие" in {
    checkName("/book/Бытие", "Genesis")
  }

  it should "contain /book/Genesis/chapter/3" in {
    checkChapterLength("/book/Genesis/chapter/3", 24)
  }

  it should "contain /book/Exodus/chapter/1" in {
    checkChapterLength("/book/Exodus/chapter/1", 22)
  }

  it should "contain /book/Exodus/chapter/1/verse/3" in {
    checkVerseNumber("/book/Exodus/chapter/1/verse/3", 3)
    checkVerseNumber("/book/Exodus/chapter/2/verse/5", 5)
  }

  it should "contain /book/Бытие/parsha/Noach" in {
    checkName("/book/Бытие/parsha/Noach", "Ноах")
  }
}
