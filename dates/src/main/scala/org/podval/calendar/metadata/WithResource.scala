package org.podval.calendar.metadata

trait WithResource {
  protected def resourceName: String = Named.className(this)
}
