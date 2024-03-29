package org.opentorah.math

import org.opentorah.util.Json
import org.opentorah.xml.ScalaXml

trait MathJax:

  // TODO MathJax documentation says that this should be in the 'head', not 'body'...
  def body(payload: ScalaXml.Nodes): Seq[ScalaXml.Element]

  final def htmlConfigurationString(math: MathConfiguration): String =
    Json.fromMap(htmlConfiguration(math))

  def htmlConfiguration(math: MathConfiguration): Map[String, Matchable]

  // TODO I only need one package (again ;))
  val npmPackagesToInstall: List[String]

  val useEsm: Boolean
  
  def nodeScript(
    math: MathConfiguration,
    mathString: String,
    input: Input,
    fontSize: Float
  ): String

object MathJax:
  def apply(math: MathConfiguration): MathJax = if math.useMathJaxV3.contains(true) then MathJax3 else MathJax2
