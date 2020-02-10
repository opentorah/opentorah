package org.digitaljudaica.xml

final class Resource private(val obj: AnyRef, name: Option[String]) {
  def nameEffective: String = name.getOrElse(org.digitaljudaica.util.Util.className(obj))
}

object Resource {
  def apply(obj: AnyRef): Resource = new Resource(obj, None)

  def apply(obj: AnyRef, name: String): Resource = new Resource(obj, Some(name))
}
