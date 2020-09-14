package org.opentorah.collector

import org.opentorah.store.{Selector, Store, Urls}
import org.opentorah.tei.Page

final class Book(
  inheritedSelectors: Seq[Selector],
  urls: Urls,
  element: Store.Inline
) extends Collection(inheritedSelectors, urls, element) {
  override def pageType: Page.Type = Page.Book
}
