package org.podval.docbook.gradle

import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import java.io.{BufferedWriter, File, FileWriter, InputStream}
import section.Section

import scala.collection.JavaConverters._

object Util {
  def documentNames(
    document: Property[String],
    documents: ListProperty[String]
  ): (Option[String], List[String]) = {
    def ifNotEmpty(string: String): Option[String] =
      if (string.isEmpty) None else Some(string)

    val documentName: Option[String] = ifNotEmpty(document.get).map(name =>
      dropAllowedExtension(name, "xml"))
    val documentNames: List[String] = documents.get.asScala.toList.flatMap(ifNotEmpty).map(name =>
      dropAllowedExtension(name, "xml"))

    if (documentName.isEmpty && documentNames.isEmpty)
      throw new IllegalArgumentException(
        """At least one document name must be specified using
          |  document = "<document name>"
          |or
          |  documents = ["<document name>"]
          |""".stripMargin)

    (documentName, documentNames)
  }

  def fileNameAndExtension(nameWithExtension: String): (String, Option[String]) = {
    val lastDot: Int = nameWithExtension.lastIndexOf(".")
    if (lastDot == -1) (nameWithExtension, None)
    else (
      nameWithExtension.substring(0, lastDot),
      Some(nameWithExtension.substring(lastDot+1))
    )
  }

  def dropAllowedExtension(nameWihtExtension: String, allowedExtension: String): String = {
    val (name: String, extension: Option[String]) = fileNameAndExtension(nameWihtExtension)
    if (extension.nonEmpty && !extension.contains("xml"))
      throw new IllegalArgumentException(s"Extension must be '$allowedExtension' if present: $nameWihtExtension")
    name
  }

  def getSections(property: MapProperty[String, java.util.Map[String, String]]): Map[Section, Map[String, String]] = {
    val result: Map[String, Map[String, String]] = property.get.asScala.toMap.mapValues(_.asScala.toMap)
    result.map { case (sectionName, parameters) =>
        Section.forName(sectionName) -> parameters
    }
  }

  def applicationString: String = {
    val info = getClass.getPackage
    info.getImplementationTitle + " version " + info.getImplementationVersion
  }

  def deleteRecursively(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles.foreach(deleteRecursively)
    if (file.exists && !file.delete)
      throw new Exception(s"Unable to delete ${file.getAbsolutePath}")
  }

  def readFrom(clazz: Class[_], name: String): String = {
    val is: InputStream = clazz.getResourceAsStream(name)
    if (is == null) {
      val message: String = s"Resource not found:  ${clazz.getCanonicalName}:$name"
      throw new IllegalArgumentException(message)
    }
    scala.io.Source.fromInputStream(is).getLines.mkString("\n")
  }

  def readFrom(file: File): String =
    scala.io.Source.fromFile(file).getLines.mkString("\n")

  def writeInto(file: File, logger: Logger, replace: Boolean)(content: => String): Unit = {
    if (!replace && file.exists) {
      logger.info(s"Already exists: $file")
    } else {
      logger.info(s"Writing $file")
      file.getParentFile.mkdirs()
      val writer: BufferedWriter = new BufferedWriter(new FileWriter(file))

      try {
        writer.write(content.stripMargin)
      } finally {
        writer.close()
      }
    }
  }

  //  def fileInputSource(file: File): InputSource =
  //    new InputSource(file.toURI.toASCIIString)

  //  def stringInputSource(input: String, publicId: Option[String], systemId: Option[String]): InputSource = {
  //    val result = new InputSource(new StringReader(input))
  //    result.setPublicId(publicId.orNull)
  //    result.setSystemId(systemId.orNull)
  //    result
  //  }

  //  def resourceInputSource(name: String): InputSource = {
  //    val result = new InputSource(getClass.getResourceAsStream(name))
  //    result.setSystemId(name)
  //    result.setPublicId(null)
  //    result
  //  }

  //  def fileSource(file: File): Source =
  //    new StreamSource(file)

  //  def stringSource(input: String, publicId: Option[String], systemId: Option[String]): Source = {
  //    val result = new StreamSource(new StringReader(input))
  //    result.setPublicId(publicId.orNull)
  //    result.setSystemId(systemId.orNull)
  //    result
  //  }
}
