package org.opentorah.docbook.plugin

import java.io.File
import org.gradle.api.Project
import org.opentorah.docbook.section.{DocBook2, CommonSection, Variant}
import org.opentorah.util.Files

// Although Gradle caches resolved artifacts and npm caches packages that it retrieves,
// unpacking frameworks under `/build` after each `./gradlew clean` takes noticeable time -
// around 14 seconds; so, I am caching unpacked frameworks under `~/.gradle`.

final class Layout(val frameworksDir: File, val projectDir: File, val buildDir: File) {
  def settingsGradle: File = new File(projectDir, "settings.gradle")
  def buildGradle: File = new File(projectDir, "build.gradle")

  // frameworks
  def frameworkDirectory(name: String): File = new File(frameworksDir, name)

  // Node: ~/.gradle/nodejs
  def nodeRoot: File = frameworkDirectory("nodejs")
  def j2v8LibraryDirectory: File = frameworkDirectory("j2v8library")

  // DocBook XSLT: ~/.gradle/docbook
  val docBookXslDirectory: File = Files.file(frameworksDir, Seq("docbook"))

  // src/main/
  private def sourceDir: File = new File(new File(projectDir, "src"), "main")
  private def sourceDirectory(name: String): File = new File(sourceDir, name)

  // src/main/docBook/
  private def inputDirectoryName: String = "docBook"
  def inputDirectory: File = sourceDirectory(inputDirectoryName)
  def inputFile(inputFileName: String): File = new File(inputDirectory, inputFileName + ".xml")

  // src/main/css/
  def cssDirectoryName: String = "css"
  def cssDirectory: File = sourceDirectory(cssDirectoryName)
  def cssFile(name: String): File = new File(cssDirectory, name + ".css")
  def cssFileRelativeToOutputDirectory(name: String): String = cssDirectoryName + "/" + name + ".css"

  // src/main/images/
  def imagesDirectoryName: String = "images"
  def imagesDirectory: File = sourceDirectory(imagesDirectoryName)

  // src/main/fop/
  private def fopConfigurationDirectoryName: String = "fop"
  def fopConfigurationDirectory: File = sourceDirectory(fopConfigurationDirectoryName)
  private def fopConfigurationFileName: String = "fop.xconf"
  // FOP configuration file is the same for XSLT 1.0 and 2.0
  def fopConfigurationFile: File = new File(fopConfigurationDirectory, fopConfigurationFileName)

  // src/main/xsl/
  def customStylesheet(commonSection: CommonSection): String = commonSection.name + "-custom.xsl"
  def customStylesheet(variant: Variant): String = variant.fullName + "-custom.xsl"
  def paramsStylesheet(variant: Variant): String = variant.fullName + "-params.xsl"
  private def stylesheetDirectoryName: String = "xsl"
  def stylesheetDirectory: File = sourceDirectory(stylesheetDirectoryName)
  def stylesheetFile(name: String) = new File(stylesheetDirectory, name)

  // src/main/xml/
  def catalogGroupBase: String = "../../.."  // to get to the project root
  private def xmlDirectory: File = sourceDirectory("xml")
  def xmlFile(name: String): File = new File(xmlDirectory, name)
  def substitutionsDtdFileName: String = "substitutions.dtd"
  private def catalogFileName: String = "catalog.xml"
  def catalogFile: File = xmlFile(catalogFileName)
  def catalogCustomFileName: String = "catalog-custom.xml"

  // src/test
  def testResource(name: String): File =
    new File(new File(new File(new File(projectDir, "src"), "test"), "resources"), name)

  // build/
  private def buildDirRelative: String =
    if (buildDir.getParentFile == projectDir) buildDir.getName
    else throw new IllegalArgumentException("buildDir that is not a child of projectDir is not yet supported! ")

  def buildDirectory(name: String): File = new File(buildDir, name)
  def buildFile(name: String): File = new File(buildDir, name)
  def buildDirectoryRelative(name: String): String = s"$buildDirRelative/$name/"

  // build/data
  private def dataDirectoryName: String = "data"
  def dataDirectory: File = buildDirectory(dataDirectoryName)
  def dataDirectoryRelative: String = buildDirectoryRelative(dataDirectoryName)

  // build/docBook
  // build/docBookTmp

  private def outputRootName: String = "docBook"
  def outputRoot: File = buildDirectory(outputRootName)

  private def intermediateRootName: String = "docBookTmp"
  def intermediateRoot: File = buildDirectory(intermediateRootName)

  def forDocument(prefixed: Boolean, documentName: String): Layout.ForDocument = new Layout.ForDocument {

    override def mainStylesheet(variant: Variant): String =
      variant.fullName +
      (if (!prefixed) "" else "-" + documentName) +
      ".xsl"

    override def outputDirectory(variant: Variant, isSaxon: Boolean): File = {
      val root: File = if (isSaxon && variant.docBook2.usesIntermediate) intermediateRoot else outputRoot
      new File(
        if (!prefixed) root else new File(root, documentName),
        variant.fullName
      )
    }

    override def outputFile(variant: Variant, isSaxon: Boolean): File = {
      val docBook2: DocBook2 = variant.docBook2
      val extension: String =
        if (isSaxon && docBook2.usesIntermediate)
          docBook2.intermediateFileExtension
        else
          docBook2.outputFileExtension

      new File(
        outputDirectory(variant, isSaxon),
        docBook2.rootFilename(documentName) + "." + extension
      )
    }
  }
}

object Layout {
  trait ForDocument {
    def mainStylesheet(variant: Variant): String

    final def saxonOutputDirectory(variant: Variant): File = outputDirectory(variant, isSaxon = true)

    final def outputDirectory(variant: Variant): File = outputDirectory(variant, isSaxon = false)

    def outputDirectory(variant: Variant, isSaxon: Boolean): File

    final def saxonOutputFile(variant: Variant): File = outputFile(variant, isSaxon = true)

    final def outputFile(variant: Variant): File = outputFile(variant, isSaxon = false)

    def outputFile(variant: Variant, isSaxon: Boolean): File
  }

  def forProject(project: Project): Layout = new Layout(
    frameworksDir = project.getGradle.getGradleUserHomeDir,
    projectDir = project.getProjectDir,
    buildDir = project.getBuildDir
  )

  def forRoot(root: File): Layout = new Layout(
    frameworksDir = new File(root, "build"),
    projectDir = root,
    buildDir = new File(root, "build")
  )

  def forCurrent: Layout =
    forRoot(new File(".").getAbsoluteFile.getParentFile)
}
