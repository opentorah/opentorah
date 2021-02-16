package org.opentorah.markdown

import com.vladsch.flexmark.Extension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.util.options.MutableDataHolder

final class DoubleEscapeHtmlRendererExtension extends HtmlRenderer.HtmlRendererExtension {
  override def rendererOptions(options: MutableDataHolder): Unit = {}

  override def extend(rendererBuilder: HtmlRenderer.Builder, rendererType: String): Unit = {
    if (rendererBuilder.isRendererType("HTML")) {
      rendererBuilder.nodeRendererFactory(new DoubleEscapeHtmlNodeRenderer.Factory())
    }
  }
}

object DoubleEscapeHtmlRendererExtension {
  def create: Extension = new DoubleEscapeHtmlRendererExtension
}
