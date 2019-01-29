package org.podval.docbook.gradle

import org.gradle.api.Project
import org.gradle.api.provider.{MapProperty, Property}

import scala.beans.BeanProperty

class DocBookExtension(project: Project) {
  @BeanProperty val documentName: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val dataGeneratorClass: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val parameters: MapProperty[String, String] =
    project.getObjects.mapProperty(classOf[String], classOf[String])
}
