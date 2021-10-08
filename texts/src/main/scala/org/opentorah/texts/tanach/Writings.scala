package org.opentorah.texts.tanach

import org.opentorah.metadata.HasName

open class Writings(nameOverride: Option[String] = None) extends Nach(nameOverride), HasName.NonEnum

// TODO make enum (including Psalms)!
object Writings:
  case object Proverbs extends Writings
  case object Job extends Writings
  case object SongOfSongs extends Writings(nameOverride = Some("Song of Songs"))
  case object Ruth extends Writings
  case object Lamentations extends Writings
  case object Ecclesiastes extends Writings
  case object Esther extends Writings
  case object Daniel extends Writings
  case object Ezra extends Writings
  case object Nehemiah extends Writings
  case object ChroniclesI extends Writings(nameOverride = Some("I Chronicles"))
  case object ChroniclesII extends Writings(nameOverride = Some("II Chronicles"))

  val all: Seq[Writings] = Seq(Psalms, Proverbs, Job, SongOfSongs, Ruth, Lamentations, Ecclesiastes,
    Esther, Daniel, Ezra, Nehemiah, ChroniclesI, ChroniclesII)
