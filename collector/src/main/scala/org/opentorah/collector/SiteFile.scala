package org.opentorah.collector

import scala.xml.Elem

trait SiteFile {

  def url: Seq[String]

  def viewer: Viewer

  def navigationLinks: Seq[NavigationLink]

  def contentElement: Elem
}
