package org.opentorah.collector

import org.opentorah.store.{Selector, Store, Urls}

final class Book(
  inheritedSelectors: Seq[Selector],
  urls: Urls,
  element: Store.Inline
) extends Collection(inheritedSelectors, urls, element) {
  override def pageType: Page.Type = Page.Book
}
