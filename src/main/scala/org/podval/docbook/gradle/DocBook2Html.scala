package org.podval.docbook.gradle

import org.gradle.api.Project

object DocBook2Html extends DocBook2 {
  override def saxon2intermediate: Boolean = false

  override def finalOutputFormat: String = "html"

  override protected def additionalParameters(layout: Layout, inputFileName: String): Map[String, String] =
    additionalParametersHtml(layout, inputFileName)

  override protected def outputFileNameOverride: Option[String] = Some("index")

  override protected def postProcess(
    layout: Layout,
    inputFileName: String,
    substitutions: Map[String, String],
    project: Project
  ): Unit = {
    copyImagesAndCss(
      layout,
      finalOutputDirectory(layout),
      substitutions,
      project
    )
  }
}
