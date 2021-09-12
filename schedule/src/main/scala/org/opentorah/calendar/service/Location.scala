package org.opentorah.calendar.service

enum Location(val name: String, val inHolyLand: Boolean):
  case HolyLand extends Location("Holy Land", true)
  case Diaspora extends Location("Diaspora", false)
