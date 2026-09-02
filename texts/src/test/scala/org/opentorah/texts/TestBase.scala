package org.opentorah.texts

import org.opentorah.store.{Path, Store, Stores}
import org.opentorah.util.Effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

abstract class TestBase(underTest: Stores[?]) extends AnyFlatSpec, Matchers:
  def resolve(path: String): Path = Effects.unsafeRun(underTest.resolve(path))
  def resolveLast(path: String): Store = resolve(path).last
  def checkName(path: String, name: String): Unit = resolveLast(path).names.hasName(name) shouldBe true
