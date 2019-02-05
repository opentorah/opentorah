package org.podval.docbook.gradle

import org.gradle.api.Project
import java.io.File
import org.apache.tools.ant.taskdefs.Zip
import org.apache.tools.ant.types.FileSet

object DocBook2Epub extends DocBook2 {
  override def saxon2intermediate: Boolean = true

  override def finalOutputFormat: String = "epub"

  override protected def additionalParameters(layout: Layout, inputFileName: String): Map[String, String] =
    additionalParametersHtml(layout, inputFileName)

  override protected def postProcess(
    layout: Layout,
    inputFileName: String,
    substitutions: Map[String, String],
    project: Project,
    logger: Logger
  ): Unit = {
    val expandedEpubDirectory: File = intermediateOutputDirectory(layout)

    copyCss(
      layout,
      expandedEpubDirectory,
      substitutions,
      project,
      logger
    )

    val zip = new Zip
    zip.setProject(new org.apache.tools.ant.Project)
    zip.setPreserve0Permissions(true)
    zip.setCompress(false)
    zip.setDestFile(finalOutputFile(layout, inputFileName))
    val fileSet = new FileSet()
    fileSet.setDir(expandedEpubDirectory)
    zip.addFileset(fileSet)
    zip.execute()
  }
}
