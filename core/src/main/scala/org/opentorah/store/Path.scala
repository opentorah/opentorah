package org.opentorah.store

import org.opentorah.metadata.Language
import org.opentorah.xml.A

type Path = Seq[Store]

object Path:
  // TODO Scala compiler gives me error if I use unfolded Path => Path in type annotations inside a for...
  type Shortener = Path => Path

  def structureNames(path: Path): Seq[String] = path.map(_.names.doFind(Language.English.toSpec).name)
  
  def last[T](path: Path): T = path.last.asInstanceOf[T]

  def takeTo[T](path: Path, clazz: Class[T]): Path =
    val cutOffIndex: Int = path.indexWhere(clazz.isInstance)
    require(cutOffIndex > 0)
    path.take(cutOffIndex+1)

  def a(path: Path, pathShortener: Shortener): A =
    A(structureNames(pathShortener(path)))
    .setTarget(path.last.viewer)
