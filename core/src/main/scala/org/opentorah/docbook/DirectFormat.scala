package org.opentorah.docbook

import org.opentorah.html.{SiteHtml, Html as XHtml}
import org.opentorah.math.MathConfiguration
import org.opentorah.xml.{PrettyPrinter, Resolver}
import java.io.File

trait DirectFormat extends Format:

  def process(
    resolver: Resolver,
    inputFile: File,
    parameters: Parameters,
    math: MathConfiguration,
    siteHtml: SiteHtml,
    processOutputFile: File
  ): Unit

object DirectFormat:
  val prettyPrinter: PrettyPrinter = XHtml.prettyPrinter + DocBook.prettyPrinter
