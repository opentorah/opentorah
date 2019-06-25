package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.util.Logger

object ExternalMathJax extends MathJax.Factory {

  override def get(
    node: Node,
    configuration: Configuration,
    logger: Logger
  ): MathJax = new MathJax(node, configuration, logger) {

    override protected def typeset(
      options: Map[String, Any],
      outputName: String,
    ): String =  node.evaluate(
      // I have to use console.error() and not console.log() so that the output gets flushed before the project exist;
      // that is why I collect both out and err in Installation.exec()...
      s"""
         |var mjAPI = require("mathjax-node");
         |mjAPI.config(${map2json(configuration.toMap)});
         |mjAPI.start();
         |mjAPI.typeset(${map2json(options)}, function (data) {
         |  if (!data.errors) { console.error(data.$outputName); }
         |});
         """.stripMargin
    )
  }

  private def map2json(map: Map[String, Any]): String =
    "{\n" + (for ((key, value) <- map.toSeq) yield "\"" + key + "\":" + value2json(value)).mkString(",\n") + "\n}"

  private def list2json(list: List[Any]): String =
    "[" + list.map(value2json).mkString(", ") + "]"

  private def value2json(value: Any): String = value match {
    case value: Map[String, Any] => map2json(value)
    case value: List[Any] => list2json(value)
    case value: String => "\"" + string2json(value) + "\""
    case other => other.toString
  }

  private def string2json(value: String): String = value
    .replace("\\", "\\\\") // first, so that newly-introduced '\' do not get escaped!
    .replace("\"", "\\\"")
    .replace("\n", "\\n")
}
