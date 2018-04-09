package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.Project
import org.gradle.api.provider.Property

class DocBookExtension(project: Project) {
  private val inputFile: Property[File] = project.getObjects.property(classOf[File])
  // TODO default - 'index.xml'?

  def getInputFile: Property[File] = inputFile
}
