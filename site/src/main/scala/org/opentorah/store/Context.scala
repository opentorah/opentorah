package org.opentorah.store

import org.opentorah.xml.Caching

trait Context {
  def pathShortener: Caching.Parser[Path.Shortener]
  
  def path(store: Store): Path
}
