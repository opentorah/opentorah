package org.podval.calendar.astronomy

import org.podval.calendar.jewish.Jewish.Day

class Calculator(val epoch: Epoch, val calculators: Calculators, val rounders: Rounders) {

  def calculate(day: Day): Calculation = new Calculation(
    calculator = this,
    day = day
  )
}


object Calculator {

  object Text extends Calculator(Epoch.Text, Calculators.Text, Rounders.Text)
}
