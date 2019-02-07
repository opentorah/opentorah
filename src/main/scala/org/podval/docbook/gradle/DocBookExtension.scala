package org.podval.docbook.gradle

import org.gradle.api.Project
import org.gradle.api.provider.{ListProperty, MapProperty, Property}

import scala.beans.BeanProperty

class DocBookExtension(project: Project) {
  @BeanProperty val documentName: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val dataGeneratorClass: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val xslParameters: MapProperty[String, String] =
    project.getObjects.mapProperty(classOf[String], classOf[String])

  @BeanProperty val substitutions: MapProperty[String, String] =
    project.getObjects.mapProperty(classOf[String], classOf[String])

  @BeanProperty val outputFormats: ListProperty[String] =
    project.getObjects.listProperty(classOf[String])

  @BeanProperty val outputFormats2: ListProperty[String] =
    project.getObjects.listProperty(classOf[String])

  @BeanProperty val epubEmbeddedFonts: ListProperty[String] =
    project.getObjects.listProperty(classOf[String])
}
