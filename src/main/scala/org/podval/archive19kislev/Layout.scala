package org.podval.archive19kislev

import java.io.File


object Layout {

  // Relies on being cloned side-by-side with the repository that contains the site - in directory "site" :)

  val master: File = {
    val cwd = new java.io.File(".").getCanonicalFile
    val result = if (cwd.getName.endsWith(".idea-project")) cwd.getParentFile else cwd
    result
  }

  val xml = new File(new File(new File(master, "src"), "main"), "xml")


  val site = new File(master.getParentFile, "site")
}
