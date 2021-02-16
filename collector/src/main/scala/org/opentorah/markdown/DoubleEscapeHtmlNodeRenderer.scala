package org.opentorah.markdown

import com.vladsch.flexmark.ast.{Code, CodeBlock, FencedCodeBlock, IndentedCodeBlock, Text}
import com.vladsch.flexmark.html.{HtmlRendererOptions, HtmlWriter}
import com.vladsch.flexmark.html.renderer.{CoreNodeRenderer, NodeRenderer, NodeRendererContext, NodeRendererFactory,
  NodeRenderingHandler}
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.html.Escaping
import com.vladsch.flexmark.util.options.DataHolder
import com.vladsch.flexmark.util.sequence.BasedSequence
import org.opentorah.markdown.DoubleEscapeHtmlNodeRenderer.doubleEscape
import scala.jdk.CollectionConverters._

object DoubleEscapeHtmlNodeRenderer {
  // Note: FlexMark HtmlRenderer escapes the tags, but when I re-parse its output into XML,
  // XML parser un-escapes them, so I force double-escaping by pre-escaping the fenced code block.
  // I am not sure why doesn't everybody need it: aren't browser's parsers supposed to un-escape default entities?
  // In order to be able to substitute this call for the html.text(string) that CoreNodeRenderer has,
  // I had to duplicate a *lot* of code from there. There must be a better way!
  private def doubleEscape(string: String, html: HtmlWriter): Unit = {
    // html.text() calls Escaping; I pre-escape once more here:
    html.text(Escaping.escapeHtml(string, false))
  }

  final class Factory extends NodeRendererFactory {
    override def create(options: DataHolder): NodeRenderer = {
      new DoubleEscapeHtmlNodeRenderer(options)
    }
  }
}

final class DoubleEscapeHtmlNodeRenderer(options: DataHolder) extends NodeRenderer {
  private val codeContentBlock: Boolean = Parser.FENCED_CODE_CONTENT_BLOCK.getFrom(options)
  private val codeSoftLineBreaks: Boolean = Parser.CODE_SOFT_LINE_BREAKS.getFrom(options)

  override def getNodeRenderingHandlers: java.util.Set[NodeRenderingHandler[_]] = Set[NodeRenderingHandler[_]](
    new NodeRenderingHandler[FencedCodeBlock](classOf[FencedCodeBlock], DoubleEscapeHtmlNodeRenderer.this.render),
    new NodeRenderingHandler[IndentedCodeBlock](classOf[IndentedCodeBlock], DoubleEscapeHtmlNodeRenderer.this.render),
    new NodeRenderingHandler[CodeBlock](classOf[CodeBlock], DoubleEscapeHtmlNodeRenderer.this.render),
    new NodeRenderingHandler[Code](classOf[Code], DoubleEscapeHtmlNodeRenderer.this.render)
  ).asJava

  private def render(node: FencedCodeBlock, context: NodeRendererContext, html: HtmlWriter): Unit = {
    html.line
    html.srcPosWithTrailingEOL(node.getChars).withAttr().tag("pre").openPre

    val info: BasedSequence = node.getInfo
    if (info.isNotNull && !info.isBlank) {
      val language: BasedSequence = node.getInfoDelimitedByAny(" ")
      html.attr("class", context.getHtmlOptions.languageClassPrefix + language.unescape())
    } else {
      val noLanguageClass: String = context.getHtmlOptions.noLanguageClass.trim
      if (noLanguageClass.nonEmpty) html.attr("class", noLanguageClass)
    }

    html.srcPosWithEOL(node.getContentChars()).withAttr(CoreNodeRenderer.CODE_CONTENT).tag("code")
    if (codeContentBlock) context.renderChildren(node)
    else doubleEscape(node.getContentChars.normalizeEOL, html)

    html.tag("/code")
    html.tag("/pre").closePre
    html.lineIf(context.getHtmlOptions.htmlBlockCloseTagEol)
  }

  private def render(node: IndentedCodeBlock, context: NodeRendererContext, html: HtmlWriter): Unit = {
    html.line()
    html.srcPosWithEOL(node.getChars).withAttr.tag("pre").openPre

    val noLanguageClass: String = context.getHtmlOptions.noLanguageClass.trim
    if (noLanguageClass.nonEmpty) html.attr("class", noLanguageClass)

    html.srcPosWithEOL(node.getContentChars).withAttr(CoreNodeRenderer.CODE_CONTENT).tag("code")
    if (codeContentBlock) context.renderChildren(node)
    else doubleEscape(node.getContentChars.trimTailBlankLines.normalizeEndWithEOL, html)

    html.tag("/code")
    html.tag("/pre").closePre()
    html.lineIf(context.getHtmlOptions.htmlBlockCloseTagEol)
  }

  private def render(node: CodeBlock, context: NodeRendererContext, html: HtmlWriter): Unit = doubleEscape(
    if (node.getParent.isInstanceOf[IndentedCodeBlock]) node.getContentChars.trimTailBlankLines.normalizeEndWithEOL
    else node.getContentChars.normalizeEOL,
    html
  )

  private def render(node: Code, context: NodeRendererContext, html: HtmlWriter): Unit = {
    val htmlOptions: HtmlRendererOptions = context.getHtmlOptions
    if (htmlOptions.codeStyleHtmlOpen == null || htmlOptions.codeStyleHtmlClose == null) {
      if (context.getHtmlOptions.sourcePositionParagraphLines) html.withAttr().tag("code")
      else html.srcPos(node.getText).withAttr().tag("code")

      if (codeSoftLineBreaks && !htmlOptions.isSoftBreakAllSpaces) {
        for (child <- node.getChildren.asScala) {
          if (child.isInstanceOf[Text]) doubleEscape(Escaping.collapseWhitespace(child.getChars, true), html)
          else context.render(child)
        }
      } else doubleEscape(Escaping.collapseWhitespace(node.getText, true), html)
      html.tag("/code")
    } else {
      html.raw(htmlOptions.codeStyleHtmlOpen)
      if (codeSoftLineBreaks && !htmlOptions.isSoftBreakAllSpaces) {
        for (child <- node.getChildren.asScala) {
          if (child.isInstanceOf[Text]) doubleEscape(Escaping.collapseWhitespace(child.getChars, true), html)
          else context.render(child)
        }
      } else doubleEscape(Escaping.collapseWhitespace(node.getText, true), html)
      html.raw(htmlOptions.codeStyleHtmlClose)
    }
  }
}

