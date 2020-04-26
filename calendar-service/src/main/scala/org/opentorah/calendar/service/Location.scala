package org.opentorah.calendar.service

abstract class Location(val name: String, val inHolyLand: Boolean)

object Location {
  case object HolyLand extends Location("Holy Land", true)

  case object Diaspora extends Location("Diaspora", false)
}
