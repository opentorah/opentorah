package org.podval.archive19kislev

import java.io.File


object Layout {

  // Relies on being cloned side-by-side with the repository that contains the site - in directory "site" :)

  val code: File = {
    val cwd = new java.io.File(".").getCanonicalFile
    val result = if (cwd.getName.endsWith(".idea-project")) cwd.getParentFile else cwd
    result
  }


  val site = new File(code.getParentFile, "site")
}
