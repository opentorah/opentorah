package org.podval.docbook.gradle

import org.gradle.api.Project
import org.gradle.api.provider.{ListProperty, MapProperty, Property}

import scala.beans.BeanProperty

// Properties are annotated with @BeanProperty to make them visible to Gradle.
class Extension(project: Project) {
  @BeanProperty val xslt1version: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val xslt2version: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val documentName: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val dataGeneratorClass: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val parameters: MapProperty[String, java.util.Map[String, String]] =
    project.getObjects.mapProperty(classOf[String], classOf[java.util.Map[String, String]])

  @BeanProperty val substitutions: MapProperty[String, String] =
    project.getObjects.mapProperty(classOf[String], classOf[String])

  @BeanProperty val outputFormats: ListProperty[String] =
    project.getObjects.listProperty(classOf[String])

  @BeanProperty val cssFileName: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val isJEuclidEnabled: Property[Boolean] =
    project.getObjects.property(classOf[Boolean])

  @BeanProperty val epubEmbeddedFonts: ListProperty[String] =
    project.getObjects.listProperty(classOf[String])
}
