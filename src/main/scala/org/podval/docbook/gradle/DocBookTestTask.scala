package org.podval.docbook.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.{Input, TaskAction}

// This class can not be 'final' - Gradle needs to create a proxy...
class DocBookTestTask extends DefaultTask {

  @TaskAction
  def test(): Unit = {
    println("**************************************************************")
  }
}
