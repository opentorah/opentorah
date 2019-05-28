package org.podval.docbook.gradle

import java.io.File

import org.podval.docbook.gradle.util.Util
import org.podval.docbook.gradle.xml.Namespace
import org.scalatest.{FlatSpec, Matchers}

class PluginTest extends FlatSpec with Matchers {

  private val documentName: String = "test"

  private def writeTestProject(
    name: String,
    substitutions: Map[String, String])(
    document: String
  ): PluginTestProject = {
    val result: PluginTestProject = new PluginTestProject(
      projectDir = new File(Fixture.getBuildDir, s"pluginTestProjects/$name")
    )
    result.writeSettingsGradle(Fixture.getProjectDir)
    result.writeBuildGradleWithDocument(
      documentName = documentName,
      document = document,
      substitutions = substitutions
    )
    result
  }

  private def test(
    name: String,
    substitutions: Map[String, String])(
    document: String)(
    inIndexHtml: String*
  ): Unit = {
    val project = writeTestProject(name, substitutions)(document)

    project.run("processDocBook")

    val indexHtmlFile: File =
      project.layout.forDocument(prefixed = false, documentName).saxonOutputFile(section.Html)

    val indexHtml: String = Util.readFrom(indexHtmlFile)

    for (string: String <- inIndexHtml)
      indexHtml.contains(string) shouldBe true
  }

  "Plugin" should "preserve the title" in test(
    name = "title",
    substitutions = Map.empty)
  {
    s"""<article ${DocBook.withVersion}>
       |  <info>
       |    <title>Test DocBook File</title>
       |  </info>
       |</article>
      """
  }(
    "Test DocBook File"
  )

  it should "resolve processing instructions and entity substitutions with DTD enabled" in test(
    name = "substitutions-with-DTD",
    substitutions = Map[String, String]("version" -> "\"v1.0.0\""))
  {
    s"""<!DOCTYPE article PUBLIC "${DocBook.dtdId}" "${DocBook.dtdUri}">
       |<article ${DocBook.withVersion} ${Namespace.XLink}>
       |  <para>Processing instruction: <?eval version ?>.</para>
       |  <para>Processing instruction with unknown substitution: <?eval version1 ?>.</para>
       |  <para>Unknown processing instruction:<?eval1 XXX ?>.</para>
       |  <para>Entity: &version;.</para>
       |  <para>Entity in an attribute:<link xlink:href="http://&version;">link!</link>.</para>
       |</article>
      """
  }(
    "Processing instruction: v1.0.0.",
    "Processing instruction with unknown substitution: Evaluation failed for [version1].",
    "Unknown processing instruction:.",
    "Entity: v1.0.0.",
    """Entity in an attribute:<a class="link" href="http://v1.0.0" target="_top">link!</a>."""
  )

  it should "resolve processing instructions substitutions without DTD enabled" in test(
    name = "substitutions-without-DTD-processing-instructions",
    substitutions = Map[String, String]("version" -> "\"v1.0.0\""))
  {
    s"""<article ${DocBook.withVersion} ${Namespace.XLink}>
       |  <para>Processing instruction: <?eval version ?>.</para>
       |  <para>Processing instruction with unknown substitution: <?eval version1 ?>.</para>
       |  <para>Unknown processing instruction:<?eval1 XXX ?>.</para>
       |</article>
      """
  }(
    "Processing instruction: v1.0.0.",
    "Processing instruction with unknown substitution: Evaluation failed for [version1].",
    "Unknown processing instruction:."
  )

  it should "fail resolving entity substitutions without DTD enabled" in {
    val project: PluginTestProject = writeTestProject(
      name = "substitutions-without-DTD-entity-substitutions",
      substitutions = Map[String, String]("version" -> "\"v1.0.0\"")) {
      s"""<article ${DocBook.withVersion} ${Namespace.XLink}>
         |  <para>Processing instruction: <?eval version ?>.</para>
         |  <para>Entity: &version;.</para>
         |  <para>Entity in an attribute:<link xlink:href="http://&version;">link!</link>.</para>
         |</article>
      """
    }

    project.fail("processDocBook").contains(
      """The entity "version" was referenced, but not declared.""") shouldBe true
  }
}
