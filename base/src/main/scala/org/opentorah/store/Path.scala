package org.opentorah.store

import org.opentorah.metadata.Language

type Path = Seq[Store]

object Path:
  def structureNames(path: Path): Seq[String] = path.map(_.names.doFind(Language.English.toSpec).name)
