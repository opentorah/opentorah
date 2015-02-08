package org.podval.judaica.viewer


import java.io.File


trait Edition extends Named {

  val work: Work


  val directory: File


  override val names: Names


  val language: String


  def storage: DirectoryStorage


  final def stylesheet: File = new File(directory, "stylesheet.css")


  final def content(div: Div, format: Selector.Format): Content = {
    val unbound = storage.content(div.path.tail, format)
    val bound = DivContent.bindWithThis(unbound, div.path.head, Languages.get(language))
    bound
  }
}
