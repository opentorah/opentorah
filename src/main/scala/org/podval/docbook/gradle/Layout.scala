package org.podval.docbook.gradle

import java.io.File

trait Layout {
  def useDocBookXslt2: Boolean

  def docBookXslConfigurationName: String

  def inputFile(inputFileName: String): File

  def stylesheetDirectoryName: String

  def stylesheetFile(name: String): File

  def imagesDirectoryName: String

  def imagesDirectory: File

  def cssDirectoryName: String

  def cssDirectory: File

  def cssFileName: String

  def cssFile: File

  def fopConfigurationFile: File

  def docBookXslDirectory: File

  def docBookXslArchiveSubdirectoryName: String

  def dataDirectory: File

  def outputDirectoryRoot: File

  def saxonOutputDirectoryRoot: File
}
