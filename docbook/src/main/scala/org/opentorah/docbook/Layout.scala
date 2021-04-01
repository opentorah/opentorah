package org.opentorah.docbook

import org.opentorah.docbook.section.{CommonSection, Variant}
import org.opentorah.util.Files
import java.io.File

// Although Gradle caches resolved artifacts and npm caches packages that it retrieves,
// unpacking frameworks under `/build` after each `./gradlew clean` takes noticeable time -
// around 14 seconds; so, I am caching unpacked frameworks under `~/.gradle`.

final class Layout(val frameworksDir: File, val projectDir: File, val buildDir: File) {
  // frameworks
  private def frameworkDirectory(name: String): File = new File(frameworksDir, name)

  // Node: ~/.gradle/nodejs
  val nodeRoot: File = frameworkDirectory("nodejs")
  val j2v8LibraryDirectory: File = frameworkDirectory("j2v8library")

  // DocBook XSLT: ~/.gradle/docbook
  val docBookXslDirectory: File = Files.file(frameworksDir, Seq("docbook"))

  // src/main/
  private val sourceDir: File = new File(new File(projectDir, "src"), "main")
  private def sourceDirectory(name: String): File = new File(sourceDir, name)

  // src/main/docBook/
  val inputDirectory: File = sourceDirectory("docBook")
  def inputFile(inputFileName: String): File = new File(inputDirectory, inputFileName + ".xml")

  // src/main/css/
  val cssDirectoryName: String = "css"
  val cssDirectory: File = sourceDirectory(cssDirectoryName)
  def cssFile(name: String): File = new File(cssDirectory, name + ".css")
  def cssFileRelativeToOutputDirectory(name: String): String = cssDirectoryName + "/" + name + ".css"

  // src/main/images/
  val imagesDirectoryName: String = "images"
  def imagesDirectory: File = sourceDirectory(imagesDirectoryName)

  // src/main/fop/
  val fopConfigurationDirectory: File = sourceDirectory("fop")
  // FOP configuration file is the same for XSLT 1.0 and 2.0
  val fopConfigurationFile: File = new File(fopConfigurationDirectory, "fop.xconf")

  // src/main/xsl/
  val stylesheetDirectory: File = sourceDirectory("xsl")
  def stylesheetFile(name: String) = new File(stylesheetDirectory, name)
  def customStylesheet(commonSection: CommonSection): String = commonSection.name + "-custom.xsl"
  def customStylesheet(variant: Variant): String = variant.fullName + "-custom.xsl"
  def paramsStylesheet(variant: Variant): String = variant.fullName + "-params.xsl"

  // src/main/xml/
  private val xmlDirectory: File = sourceDirectory("xml")
  def xmlFile(name: String): File = new File(xmlDirectory, name)
  val catalogGroupBase: String = "../../.." // to get to the project root // Files.relativize(dataDirectory, xmlDirectory),
  val substitutionsDtdFileName: String = "substitutions.dtd"
  val catalogFile: File = xmlFile("catalog.xml")
  val catalogCustomFileName: String = "catalog-custom.xml"

  // build/
  private def buildDirectory(name: String): File = new File(buildDir, name)
  val buildDirRelative: String =
    if (buildDir.getParentFile == projectDir) buildDir.getName
    else throw new IllegalArgumentException("buildDir that is not a child of projectDir is not yet supported! ")

  // build/data
  def dataDirectoryName: String = "data"
  val dataDirectory: File = buildDirectory(dataDirectoryName)
  val dataDirectoryRelative: String = s"$buildDirRelative/$dataDirectoryName/"

  // build/docBook
  // build/docBookTmp

  val outputRoot: File = buildDirectory("docBook")
  val intermediateRoot: File = buildDirectory("docBookTmp")

  def forDocument(prefixed: Boolean, documentName: String): Layout.ForDocument = new Layout.ForDocument(prefixed, documentName) {
    override protected def root(isIntermediate: Boolean): File = if (isIntermediate) intermediateRoot else outputRoot
  }

  // build/docBookDirect
  val directOutputRoot: File = buildDirectory("docBookDirect")
}

object Layout {
  abstract class ForDocument(prefixed: Boolean, documentName: String) {
    final def mainStylesheet(variant: Variant): String =
      variant.fullName + (if (!prefixed) "" else "-" + documentName) + ".xsl"

    final def saxonOutputDirectory(variant: Variant): File = outputDirectory(variant, isSaxon = true)

    final def outputDirectory(variant: Variant): File = outputDirectory(variant, isSaxon = false)

    private def outputDirectory(variant: Variant, isSaxon: Boolean): File =
      outputDirectory(isIntermediate(variant, isSaxon), variant.fullName)

    def outputDirectory(isIntermediate: Boolean, name: String): File = new File(
      Files.prefixedDirectory(
        directory = root(isIntermediate),
        prefix = if (!prefixed) None else Some(documentName)
      ),
      name
    )

    final def saxonOutputFile(variant: Variant): File = outputFile(variant, isSaxon = true)

    final def outputFile(variant: Variant): File = outputFile(variant, isSaxon = false)

    private def outputFile(variant: Variant, isSaxon: Boolean): File = new File(
      outputDirectory(variant, isSaxon),
      variant.docBook2.rootFileNameWithExtension(documentName, isIntermediate(variant, isSaxon))
    )

    private def isIntermediate(variant: Variant, isSaxon: Boolean): Boolean =
      isSaxon && variant.docBook2.usesIntermediate

    protected def root(isIntermediate: Boolean): File
  }

  def forRoot(root: File): Layout = new Layout(
    frameworksDir = new File(root, "build"),
    projectDir = root,
    buildDir = new File(root, "build")
  )

  def forCurrent: Layout =
    forRoot(new File(".").getAbsoluteFile.getParentFile)
}
