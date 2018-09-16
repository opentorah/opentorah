package org.podval.calendar.metadata

trait WithMetadata extends Named {

  type Metadata

  protected def toMetadata: Key => Metadata
}
