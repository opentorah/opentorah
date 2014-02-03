package org.podval.judaica.viewer


import java.io.File


trait Edition extends Named {

  val work: Work


  val directory: File


  override val names: Names


  // TODO add language attribute


  def storage: DirectoryStorage


  final def stylesheet: File = new File(directory, "stylesheet.css")


  final def content(div: Div, format: Selector.Format): Content = storage.content(div.path, format)
}
