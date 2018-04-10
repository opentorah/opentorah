package org.podval.docbook.gradle

import org.gradle.api.Project
import org.gradle.api.provider.Property

class DocBookExtension(project: Project) {
  private val inputFileName: Property[String] = project.getObjects.property(classOf[String])
  inputFileName.set("index")

  // TODO auto-remove ".xml"?
  def getInputFileName: Property[String] = inputFileName
}
