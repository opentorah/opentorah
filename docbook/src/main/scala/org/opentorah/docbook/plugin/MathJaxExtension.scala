package org.opentorah.docbook.plugin

import org.gradle.api.Project
import org.gradle.api.provider.{ListProperty, Property}

class MathJaxExtension @javax.inject.Inject() (project: Project) {
  private val enabled: Property[Boolean] = project.getObjects.property(classOf[Boolean])
  final def getEnabled(): Property[Boolean] = enabled

  private val useMathJax3: Property[Boolean] = project.getObjects.property(classOf[Boolean])
  final def getUseMathJax3(): Property[Boolean] = useMathJax3

  private val nodeVersion: Property[String] = project.getObjects.property(classOf[String])
  final def getNodeVersion(): Property[String] = nodeVersion

  private val useJ2V8: Property[Boolean] = project.getObjects.property(classOf[Boolean])
  final def getUseJ2V8(): Property[Boolean] = useJ2V8

  private val font: Property[String] = project.getObjects.property(classOf[String])
  final def getFont(): Property[String] = font

  private val extensions: ListProperty[String] = project.getObjects.listProperty(classOf[String])
  final def getExtensions(): ListProperty[String] = extensions

  private val texDelimiter: Property[String] = project.getObjects.property(classOf[String])
  final def getTexDelimiter(): Property[String] = texDelimiter

  private val texInlineDelimiter: Property[String] = project.getObjects.property(classOf[String])
  final def getTexInlineDelimiter(): Property[String] = texInlineDelimiter

  private val asciiMathDelimiter: Property[String] = project.getObjects.property(classOf[String])
  final def getAsciiMathDelimiter(): Property[String] = asciiMathDelimiter

  private val processEscapes: Property[Boolean] = project.getObjects.property(classOf[Boolean])
  final def getProcessEscapes(): Property[Boolean] = processEscapes
}
