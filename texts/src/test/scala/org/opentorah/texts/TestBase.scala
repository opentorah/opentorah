package org.opentorah.texts

import org.opentorah.store.{Caching, Store, Stores}
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.Parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestBase(underTest: Stores[Store]) extends AnyFlatSpec, Matchers:
  // TODO is such setup needed only for tests - or should I abstract it?
  val caching: Caching.Simple = new Caching.Simple

  def resolve(path: String): zio.Task[Store.Path] =
    Parser.toTask(Caching.provide(caching, Stores.resolve(Files.splitAndDecodeUrl(path), underTest)))

  def doResolve(path: String): Store.Path = Effects.unsafeRun(resolve(path))
  def doResolveLast(path: String): Store = doResolve(path).last
  def checkName(path: String, name: String): Unit = doResolveLast(path).names.hasName(name) shouldBe true
