package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.FromUrl

trait Store extends FromUrl.With {
  def fromUrl: FromUrl
  def names: Names
  def title: Option[Title.Value]
  def storeAbstract: Option[Abstract.Value]
  def body: Option[Body.Value]
}
