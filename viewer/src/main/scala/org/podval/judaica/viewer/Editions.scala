package org.podval.judaica.viewer

sealed trait Editions {
  def editions: Seq[Edition]
  def isNo: Boolean = false

  def content(div: Div, format: Selector.Format): Content
}

object NoEditions extends Editions {
  override def editions: Seq[Edition] = Seq.empty
  override def isNo: Boolean = true

  override def content(div: Div, format: Selector.Format): Content =
    throw new ViewerException("Edition is required for content retrieval!")
}

final class LinearEditions(override val editions: Seq[Edition]) extends Editions {
  override def content(div: Div, format: Selector.Format): Content =
    merge(editions.map(edition => (edition, edition.content(div, format))))

  def merge(contents: Seq[(Edition, Content)]): Content = ???
}

final class DiffEdition(val edition1: Edition, val edition2: Edition) extends Editions {
  override def editions: Seq[Edition] = Seq(edition1, edition2)
  override def content(div: Div, format: Selector.Format): Content = diff(edition1.content(div, format), edition2.content(div, format))

  def diff(content1: Content, content2: Content): Content = ???
}

final class SingleEdition(val edition: Edition) extends Editions {
  override def editions: Seq[Edition] = Seq(edition)

  override def content(div: Div, format: Selector.Format): Content = edition.content(div, format)
}


object Editions {

  def apply(workName: String, editionNames: String): Editions = apply(Works.getWorkByName(workName), editionNames)

  def apply(work: Work, editionNames: String): Editions = {
    if (editionNames.contains('+')) {
      val names: Seq[String] = editionNames.split('+').toIndexedSeq
      new LinearEditions(names.map(work.getEditionByName))

    } else if (editionNames.contains('-')) {
      val diffs = editionNames.split("-").toIndexedSeq
      if (diffs.length != 2) throw new ViewerException(s"$editionNames must be two names separated by -")
      new DiffEdition(work.getEditionByName(diffs(0)), work.getEditionByName(diffs(1)))

    } else {
      new SingleEdition(work.getEditionByName(editionNames))
    }
  }
}
