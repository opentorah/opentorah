package org.opentorah.texts

import org.podval.store.{Path, Store, Stores}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

abstract class TestBase(underTest: Stores[?]) extends AnyFlatSpec, Matchers:
  def resolve(path: String): Path = underTest.resolve(path)
  def resolveLast(path: String): Store = resolve(path).last
  def checkName(path: String, name: String): Unit = resolveLast(path).names.hasName(name) shouldBe true
