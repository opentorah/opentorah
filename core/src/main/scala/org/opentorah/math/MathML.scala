package org.opentorah.math

import org.opentorah.xml.{Dialect, Namespace, PrettyPrinter}

object MathML extends Dialect :

  val math: String = "math"
  val mrow: String = "mrow"
  val mi: String = "mi"
  
  override val namespace: Namespace = Namespace(uri = "http://www.w3.org/1998/Math/MathML", prefix = "mathml")

  override val mimeType: String = "application/mathml+xml"

  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    alwaysStackElements = Set(math, mrow, mi)
  )
