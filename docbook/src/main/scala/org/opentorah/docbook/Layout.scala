package org.opentorah.docbook

import org.opentorah.docbook.section.{CommonSection, Variant}
import org.opentorah.util.Files
import java.io.File

// Although Gradle caches resolved artifacts and npm caches packages that it retrieves,
// unpacking frameworks under `/build` after each `./gradlew clean` takes noticeable time -
// around 14 seconds; so, I am caching unpacked frameworks under `~/.gradle`.

final class Layout(
  frameworksDir: File,
  val projectDir: File,
  val buildDir: File,
  catalogDirectoryOverride: Option[File],
  outputRootOverride: Option[File]
) {
  // frameworks
  private def frameworkDirectory(name: String): File = new File(frameworksDir, name)
  val nodeRoot: File = frameworkDirectory("nodejs")
  val j2v8LibraryDirectory: File = frameworkDirectory("j2v8library")
  val docBookXslDirectory: File = Files.file(frameworksDir, Seq("docbook"))

  // build/
  private def buildDirectory(name: String): File = new File(buildDir, name)
  // src/main/
  private def sourceDirectory(name: String): File = Files.file(projectDir, Seq("src", "main", name))

  // Catalog
  val catalogDirectory: File = catalogDirectoryOverride.getOrElse(sourceDirectory("xml"))
  private def catalogFile(name: String): File = Files.file(catalogDirectory, Seq(name))
  val dtdFile: File = catalogFile("substitutions.dtd")
  val catalogFile: File = catalogFile("catalog.xml")
  val customCatalogFile: File = catalogFile("catalog-custom.xml")

  // build/docBook
  // build/docBookTmp

  val intermediateRoot: File = buildDirectory("docBookTmp")
  val outputRoot: File = outputRootOverride.getOrElse(buildDirectory("docBook"))

  def forDocument(prefixed: Boolean, documentName: String): Layout.ForDocument = new Layout.ForDocument(prefixed, documentName) {
    override protected def inputDirectory: File = Layout.this.inputDirectory
    override protected def stylesheetDirectory: File = Layout.this.stylesheetDirectory
    override protected def root(isIntermediate: Boolean): File = if (isIntermediate) intermediateRoot else outputRoot
  }

  // build/data
  val dataDirectory: File = buildDirectory("data")

  // src/main/docBook/
  val inputDirectory: File = sourceDirectory("docBook")

  // src/main/css/
  val cssDirectoryName: String = "css"
  val cssDirectory: File = sourceDirectory(cssDirectoryName)
  def cssFile(name: String): File = new File(cssDirectory, name + ".css")
  def cssFileRelativeToOutputDirectory(name: String): String = cssDirectoryName + "/" + name + ".css" // TODO eliminate

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
}

object Layout {
  def buildDir(root: File): File = new File(root, "build")

  def buildDirectory(root: File, name: String): File = new File(buildDir(root), name)

  def forRoot(
    root: File,
    catalogDirectoryOverride: Option[File] = None,
    outputRootOverride: Option[File] = None
  ): Layout = new Layout(
    frameworksDir = buildDir(root),
    projectDir = root,
    buildDir = buildDir(root),
    catalogDirectoryOverride = catalogDirectoryOverride,
    outputRootOverride = outputRootOverride
  )

  def forCurrent: Layout =
    forRoot(new File(".").getAbsoluteFile.getParentFile)

  abstract class ForDocument(prefixed: Boolean, documentName: String) {
    final def mainStylesheetFile(variant: Variant): File =
      stylesheetFile(variant.fullName + (if (!prefixed) "" else "-" + documentName) + ".xsl")

    final def inputFile: File = new File(inputDirectory, documentName + ".xml")

    private def stylesheetFile(name: String) = new File(stylesheetDirectory, name)

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

    protected def inputDirectory: File

    protected def stylesheetDirectory: File

    protected def root(isIntermediate: Boolean): File
  }
}
