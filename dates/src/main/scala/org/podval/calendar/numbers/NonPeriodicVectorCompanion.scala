package org.podval.calendar.numbers

trait NonPeriodicVectorCompanion[S <: NonPeriodicNumbers[S]] extends VectorCompanion[S]
  with NonPeriodicNumberCompanion[S, S#Vector]
