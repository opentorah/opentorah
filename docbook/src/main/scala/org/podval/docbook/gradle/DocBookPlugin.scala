package org.podval.docbook.gradle

import org.gradle.api.{Plugin, Project}

final class DocBookPlugin extends Plugin[Project] {
  def apply(project: Project): Unit = {
    project.getTasks.create("docbookTest", classOf[DocBookTestTask])
  }
}
