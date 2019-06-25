package org.podval.docbook.gradle.plugin

import org.gradle.api.Project
import org.gradle.api.provider.{ListProperty, Property}

import scala.beans.BeanProperty

class MathJaxExtension @javax.inject.Inject() (project: Project) {
  @BeanProperty val isEnabled: Property[Boolean] =
    project.getObjects.property(classOf[Boolean])

  @BeanProperty val useJ2V8: Property[Boolean] =
    project.getObjects.property(classOf[Boolean])

  @BeanProperty val font: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val extensions: ListProperty[String] =
    project.getObjects.listProperty(classOf[String])

  @BeanProperty val texDelimiter: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val texInlineDelimiter: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val asciiMathDelimiter: Property[String] =
    project.getObjects.property(classOf[String])
}
