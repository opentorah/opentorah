package org.podval.docbook.gradle

import java.io.File

object Fixture {
  def getTestResources: File = new File(getProjectDir, "src/test/resources")
  def getBuildDir: File = new File(getProjectDir, "build")
  def getProjectDir: File = new File(".").getAbsoluteFile
}
