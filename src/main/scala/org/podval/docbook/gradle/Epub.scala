package org.podval.docbook.gradle

import org.apache.tools.ant.taskdefs.Zip
import org.apache.tools.ant.types.FileSet

import java.io.File

object Epub {
  def pack(inputDirectory: File, outputFile: File): Unit = {
    val zip = new Zip
    zip.setProject(new org.apache.tools.ant.Project)
    zip.setPreserve0Permissions(true)
    zip.setCompress(false)
    zip.setDestFile(outputFile)
    val fileSet = new FileSet()
    fileSet.setDir(inputDirectory)
    zip.addFileset(fileSet)
    zip.execute()
  }
}
