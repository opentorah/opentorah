package org.podval.docbook.gradle

import java.io.File

trait ForXslt {
  def docBookXslConfigurationName: String

  def stylesheetUri: String

  def stylesheetDirectoryName: String

  def stylesheetDirectory: File

  def stylesheetFile(name: String): File

  def docBookXslDirectory: File

  def docBookXslArchiveSubdirectoryName: String

  def outputDirectoryRoot: File

  def saxonOutputDirectoryRoot: File
}
