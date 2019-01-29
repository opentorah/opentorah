package org.podval.docbook.gradle

import org.gradle.api.{DefaultTask, Project}
import org.gradle.api.provider.Property
import org.gradle.api.tasks.{InputFile, TaskAction}
import java.io.File

import scala.beans.BeanProperty

object ListFontsTask {
  def apply(
    project: Project,
    name: String
  ): ListFontsTask = project.getTasks.create(name, classOf[ListFontsTask])
}

class ListFontsTask extends DefaultTask {
  @InputFile @BeanProperty val configurationFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @TaskAction
  def list(): Unit = {
    org.apache.fop.tools.fontlist.FontListMain.main(Array("-c", configurationFile.get.getAbsolutePath))
  }
}
