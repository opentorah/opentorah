package org.opentorah.docbook.plugin

import org.gradle.api.{Action, Project}
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.opentorah.mathjax.MathJaxConfiguration
import org.opentorah.docbook.section.DocBook2
import scala.jdk.CollectionConverters.*

class Extension(project: Project):
  private val xslt1version: Property[String] = project.getObjects.property(classOf[String])
  final def getXslt1version(): Property[String] = xslt1version

  private val xslt2version: Property[String] = project.getObjects.property(classOf[String])
  final def getXslt2version(): Property[String] = xslt2version

  private val document: Property[String] = project.getObjects.property(classOf[String])
  final def getDocument(): Property[String] = document

  private val documents: ListProperty[String] = project.getObjects.listProperty(classOf[String])
  final def getDocuments(): ListProperty[String] = documents

  private val dataGeneratorClass: Property[String] = project.getObjects.property(classOf[String])
  final def getDataGeneratorClass(): Property[String] = dataGeneratorClass

  private val parameters: MapProperty[String, java.util.Map[String, String]] =
    project.getObjects.mapProperty(classOf[String], classOf[java.util.Map[String, String]])
  final def getParameters(): MapProperty[String, java.util.Map[String, String]] = parameters

  private val substitutions: MapProperty[String, String] =
    project.getObjects.mapProperty(classOf[String], classOf[String])
  final def getSubstitutions(): MapProperty[String, String] = substitutions

  private val outputFormats: ListProperty[String] = project.getObjects.listProperty(classOf[String])
  final def getOutputFormats(): ListProperty[String] = outputFormats

  private val cssFile: Property[String] = project.getObjects.property(classOf[String])
  final def getCssFile(): Property[String] = cssFile

  private val jEuclidEnabled: Property[Boolean] = project.getObjects.property(classOf[Boolean])
  final def getJEuclidEnabled(): Property[Boolean] = jEuclidEnabled

  private val epubEmbeddedFonts: ListProperty[String] = project.getObjects.listProperty(classOf[String])
  final def getEpubEmbeddedFonts(): ListProperty[String] = epubEmbeddedFonts

  val mathJax: MathJaxExtension = project.getObjects.newInstance(classOf[MathJaxExtension], project)

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
  jEuclidEnabled.set(false)
  epubEmbeddedFonts.set(List.empty.asJava)

  mathJax.getEnabled().set(false)
  mathJax.getUseMathJax3().set(false)
  mathJax.getNodeVersion().set("14.1.0")
  mathJax.getUseJ2V8().set(false)
  mathJax.getFont().set(MathJaxConfiguration.defaultFont)
  mathJax.getExtensions().set(List.empty.asJava)
  mathJax.getTexDelimiter().set("$$")
  mathJax.getTexInlineDelimiter().set("$")
  mathJax.getAsciiMathDelimiter().set("`")
  mathJax.getProcessEscapes().set(true)
