package org.podval.archive19kislev

import java.io.File


object Layout {

  val master: File = {
    val cwd = new java.io.File(".").getCanonicalFile
    val result = if (cwd.getName.endsWith(".idea-project")) cwd.getParentFile else cwd
    result
  }

  val xml = new File(new File(new File(master, "src"), "main"), "xml")


  val ghPages = new File(master.getParentFile, "gh-pages")
  val site = ghPages
}
