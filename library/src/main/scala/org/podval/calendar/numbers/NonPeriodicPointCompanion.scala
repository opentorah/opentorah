package org.podval.calendar.numbers

trait NonPeriodicPointCompanion[S <: NonPeriodicNumbers[S]] extends PointCompanion[S]
  with NonPeriodicNumberCompanion[S, S#Point]
