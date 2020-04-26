package org.opentorah.calendar.dates

final class MonthDescriptorBase[C <: Calendar[C]]
  (val name: C#MonthName, val length: Int, val daysBefore: Int)
