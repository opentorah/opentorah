package org.opentorah.texts

import org.opentorah.store.{Path, Store, Stores}
import org.opentorah.xml.{Caching, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

abstract class TestBase(underTest: Stores[?]) extends AnyFlatSpec, Matchers:
  val caching: Caching.Simple = new Caching.Simple
  caching.logEnabled = false

  def resolve(path: String): Path = Parser.unsafeRun(underTest.resolve(path), caching)
  def resolveLast(path: String): Store = resolve(path).last
  def checkName(path: String, name: String): Unit = resolveLast(path).names.hasName(name) shouldBe true
