package org.opentorah.math

final class DelimitersAndInput(val delimiters: Delimiters, val input: Input):
  def start: String = delimiters.start
  def end: String = delimiters.end
