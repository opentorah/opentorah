package org.opentorah.texts.tanach

trait Writings extends Nach

object Writings {
  case object Proverbs extends Writings
  case object Job extends Writings
  case object SongOfSongs extends Writings { override def name: String = "Song of Songs" }
  case object Ruth extends Writings
  case object Lamentations extends Writings
  case object Ecclesiastes extends Writings
  case object Esther extends Writings
  case object Daniel extends Writings
  case object Ezra extends Writings
  case object Nehemiah extends Writings
  case object ChroniclesI extends Writings { override def name: String = "I Chronicles" }
  case object ChroniclesII extends Writings { override def name: String = "II Chronicles" }

  val all: Seq[Writings] = Seq(Psalms, Proverbs, Job, SongOfSongs, Ruth, Lamentations, Ecclesiastes,
    Esther, Daniel, Ezra, Nehemiah, ChroniclesI, ChroniclesII)
}
