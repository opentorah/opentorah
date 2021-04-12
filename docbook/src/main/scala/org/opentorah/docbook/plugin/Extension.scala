package org.opentorah.docbook.plugin

import org.gradle.api.{Action, Project}
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.opentorah.mathjax.MathJaxConfiguration
import org.opentorah.docbook.section.DocBook2
import java.io.File
import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._

// Properties are annotated with @BeanProperty to make them visible to Gradle.
class Extension(project: Project) {
  @BeanProperty val xslt1version: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val xslt2version: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val document: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val documents: ListProperty[String] =
    project.getObjects.listProperty(classOf[String])

  @BeanProperty val dataGeneratorClass: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val parameters: MapProperty[String, java.util.Map[String, String]] =
    project.getObjects.mapProperty(classOf[String], classOf[java.util.Map[String, String]])

  @BeanProperty val substitutions: MapProperty[String, String] =
    project.getObjects.mapProperty(classOf[String], classOf[String])

  @BeanProperty val outputFormats: ListProperty[String] =
    project.getObjects.listProperty(classOf[String])

  @BeanProperty val cssFile: Property[String] =
    project.getObjects.property(classOf[String])

  @BeanProperty val isJEuclidEnabled: Property[Boolean] =
    project.getObjects.property(classOf[Boolean])

  @BeanProperty val siteFile: Property[File] =
    project.getObjects.property(classOf[File])

  @BeanProperty val epubEmbeddedFonts: ListProperty[String] =
    project.getObjects.listProperty(classOf[String])

  val mathJax: MathJaxExtension =
    project.getObjects.newInstance(classOf[MathJaxExtension], project)

  // Note: this hooks the sub-extension in:
  def mathJax(action: Action[MathJaxExtension]): Unit = action.execute(mathJax)

  // Defaults
  xslt1version.set("+")
  xslt2version.set("+")
  document.set("")
  documents.set(List.empty.asJava)
  dataGeneratorClass.set("")
  outputFormats.set(DocBook2.all.filterNot(_.usesDocBookXslt2).map(_.name).asJava)
  cssFile.set("docBook")
  isJEuclidEnabled.set(false)
  siteFile.set(Extension.dummySiteFile)
  epubEmbeddedFonts.set(List.empty.asJava)

  mathJax.isEnabled.set(false)
  mathJax.nodeVersion.set("14.1.0")
  mathJax.useJ2V8.set(false)
  mathJax.font.set(MathJaxConfiguration.defaultFont)
  mathJax.extensions.set(List.empty.asJava)
  mathJax.texDelimiter.set("$$")
  mathJax.texInlineDelimiter.set("$")
  mathJax.asciiMathDelimiter.set("`")
  mathJax.processEscapes.set(true)
}

object Extension {
  val dummySiteFile: File = new File("/no-such-file")
}
