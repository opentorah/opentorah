package org.podval.calendar.generate

trait Formatter {
  def formatLesson(
    dayNumberInMonth: Int,
    gMonthNumber: Int,
    gDayNumberInMonth: Int,
    lesson: String
  ): String

  def formatLine(line: String): String

  def numColumns: Int
}


object Formatter {
  val wide = new Formatter {
    override def formatLesson(
      dayNumberInMonth: Int,
      gMonthNumber: Int,
      gDayNumberInMonth: Int,
      lesson: String
    ): String = f"$dayNumberInMonth%2d ($gMonthNumber%2d/$gDayNumberInMonth%2d) $lesson%-3s"

    override def formatLine(line: String): String = f"$line%-14s"

    override def numColumns: Int = 4
  }

  val narrow = new Formatter {
    override def formatLesson(
      dayNumberInMonth: Int,
      gMonthNumber: Int,
      gDayNumberInMonth: Int,
      lesson: String
    ): String = f"$dayNumberInMonth%2d $lesson%-3s"

    override def formatLine(line: String): String = f"${line.take(6)}%-6s"

    override def numColumns: Int = 8
  }
}
