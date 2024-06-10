package org.opentorah.store

import org.opentorah.html.A
import org.opentorah.xml.Caching

trait Context:
  // TODO eliminate?
  final def a(store: Store): Caching.Parser[A] = a(path(store))
  protected def path(store: Store): Path

  // TODO move implementation up into the Site?
  final def a(path: Path): Caching.Parser[A] =
    for pathShortener: Path.Shortener <- pathShortener
    yield A(Path.structureNames(pathShortener(path))).setTarget(viewer(path.last))

  // TODO move up into the Site?
  protected def pathShortener: Caching.Parser[Path.Shortener]
  def viewer(store: Store): String
