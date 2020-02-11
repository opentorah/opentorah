package org.digitaljudaica.xml

import java.io.{File, FileInputStream}
import java.net.URL
import org.xml.sax.InputSource
import scala.xml.{Elem, Utility, XML}

trait From {
  final def load: From.Result = (this, loadElem)

  final def doLoad: Elem = {
    val result = load
    result._2 match {
      case Right(elem) => elem
      case Left(error) => throw new IllegalArgumentException(s"Error in ${result._1}: $error")
    }
  }

  protected def loadElem: ErrorOr[Elem]

  protected final def fromUrl(url: URL): ErrorOr[Elem] =
    fromInputSource(new InputSource(url.openStream()))

  protected final def fromInputSource(source: InputSource): ErrorOr[Elem] = try {
    Right(Utility.trimProper(XML.load(source)).asInstanceOf[Elem])
  } catch {
    case e: java.io.FileNotFoundException => Left(e.getMessage)
    case e: org.xml.sax.SAXParseException => Left(e.getMessage)
  }
}

object From {

  type Result = (From, ErrorOr[Elem])

  final case class FromXml(description: String, elem: Elem) extends From {
    override protected def loadElem: ErrorOr[Elem] = Right(elem)
  }

  final case class FromUrl(url: URL) extends From {
    override protected def loadElem: ErrorOr[Elem] = fromUrl(url)
  }

  final case class FromFile(file: File) extends From {
    override protected def loadElem: ErrorOr[Elem] =
      fromInputSource(new InputSource(new FileInputStream(file)))
  }

  object FromFile {
    def apply(directory: File, fileName: String): FromFile =
      FromFile(new File(directory, fileName + ".xml"))
  }

  final case class FromResource(obj: AnyRef, name: Option[String]) extends From {
    def nameEffective: String = name.getOrElse(org.digitaljudaica.util.Util.className(obj))

    override protected def loadElem: ErrorOr[Elem] = {
      val clazz = obj.getClass
      val name = nameEffective + ".xml"

      Option(clazz.getResource(name)).fold[ErrorOr[Elem]](Left("Resource not found"))(fromUrl)
    }
  }

  object FromResource {
    def apply(obj: AnyRef): FromResource = new FromResource(obj, None)

    def apply(obj: AnyRef, name: String): FromResource = new FromResource(obj, Some(name))
  }
}
