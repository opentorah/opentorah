package org.opentorah.html

import org.opentorah.math.MathConfiguration
import org.opentorah.xml.{Attribute, Dialect, Doctype, Namespace, PrettyPrinter, ScalaXml, XLink}
import org.opentorah.util.Json

object Html extends Dialect, Doctype:

  override val namespace: Namespace = Namespace(uri = "http://www.w3.org/1999/xhtml", prefix = "xhtml")

  override val mimeType: String = "text/html" // Note: and not "application/xhtml+xml"

  override val doctype: String = Doctype.string("html")

  val idAttribute: Attribute[String] = Attribute("id")
  val langAttribute: Attribute[String] = Attribute("lang")
  val classAttribute: Attribute[String] = Attribute("class")

  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    alwaysStackElements =
      Set("nav", "header", "main", "div"),
    clingyElements =
      Set("a"),
    // Some elements are mis-processed when they are empty, e.g. <script .../> ...
    allowEmptyElements = false,
    // ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    // and in general, it's weird to expand the elements that are always empty:
    keepEmptyElements = Set("br", "meta", "link", "img", "data"),
    preformattedElements = Set("pre")
  )

  /*
  I tried to define CSS namespaces like this:
    @namespace tei   url("http://www.tei-c.org/ns/1.0");
    @namespace db    url("http://docbook.org/ns/docbook");
    @namespace xhtml url("http://www.w3.org/1999/xhtml");
  and use them in CSS rules like this: tei|div, docbook|title.

  It seems that in browser DOM all elements are in the HTML5 xhtml namespace
  unless xmlns attribute is present on that element;
  why are the namespace declarations not inherited is not clear.

  So, I prefix the names of the elements from non-HTML namespaces with the namespace prefix
  if their names clash with the HTML namespace in a way that makes CSS styling difficult.
  For instance, I use <div> to structure the layout, but need to be able to style TEI
  depending on the level of nesting of TEI divs.
  Also, HTML disallows tables within paragraphs, so to have a tooltip inside a TEI paragraph,
  it needs to not be an HTML <p> (and of course, namespace is ignored...)
  */
  val reservedElements: Set[String] = Set("head", "body", "title", "div", "p")

  val reservedAttributes: Set[String] = Set("class", "target", "lang", "frame")

  val viewerDefault: String = "hierarchyViewer"

  val styleDefault: String = "main"

  def fullContent(
    wrapperCssClass: String,
    header: Option[ScalaXml.Element],
    bodyTitle: Option[ScalaXml.Nodes],
    content: ScalaXml.Element
  ): ScalaXml.Element =
    <div class={wrapperCssClass}>
      {ScalaXml.optional(header)((header: ScalaXml.Element) =>
      header)}
      {ScalaXml.optional(bodyTitle)((title: ScalaXml.Nodes) =>
      <header class="post-header">
        <h1 class="post-title">
          {title}
        </h1>
      </header>)}
      <div class="post-content">
        {content}
      </div>
    </div>

  // TODO copy SEO stuff from Minima...
  // TODO consolidate css, js etc. under 'asset'
  def toHtml( // TODO rename
    siteHtml: SiteHtml,
    math: MathConfiguration,
    headTitle: Option[String],
    cssFileName: String,
    viewer: String,
    navigationLinks: Seq[ScalaXml.Element], // normally, <a>s
    content: ScalaXml.Element
  ): ScalaXml.Element =
    val isMathJaxEnabled: Boolean = math.mathJaxEnabled.contains(true) && Html.isMathPresent(content)
    val isHighlighterEnabled: Boolean = siteHtml.getHighlighter.isEnabled && Html.isCodePresent(content)

    <html>
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>{ScalaXml.optional(headTitle)(title =>
        <title>
          {title}
        </title>)}
        <link rel="stylesheet" href={s"/css/$cssFileName.css"}/>
        {ScalaXml.optional(siteHtml.favicon)(favicon => <link rel="icon" href={s"/$favicon"}/>)}
        {ScalaXml.conditional(isHighlighterEnabled)(siteHtml.getHighlighter.head)}
      </head>
      <body>
        <header class="site-header" role="banner">
          <div class="wrapper">
            {ScalaXml.optional(siteHtml.title)(title =>
            <a class="site-title" rel="author" target={Html.viewerDefault} href="/">
              {title.content.scalaXml}
            </a>)}
            <nav class="site-nav">
            <input type="checkbox" id="nav-trigger" class="nav-trigger"/>
            <label for="nav-trigger">
              <span class="menu-icon">
                <svg viewBox="0 0 18 15" width="18px" height="15px">
                  <path d="M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z"/>
                </svg>
              </span>
            </label>
            <div class="trigger">
              {navigationLinks.map(pageLinkClass.set(ScalaXml))}
            </div>
          </nav>
          </div>
        </header>
        <main class="page-content" aria-label="Content">
          <div class="wrapper">
            <article class="post">
              {content}
            </article>
          </div>
        </main>
        <footer class="site-footer h-card">
          <data class="u-url" href="/"/>
          <div class="wrapper">
            <div class="footer-col-wrapper">
              <div class="footer-col footer-col-1">
                <ul class="contact-list">
                  {ScalaXml.optional(siteHtml.url)(url =>
                  <li class="p-name">
                    {url}
                  </li>)}
                  {ScalaXml.optional(siteHtml.email)(email =>
                  <li>
                    <a class="u-email" href={s"mailto:$email"}>
                      {email}
                    </a>
                  </li>)}
                </ul>
              </div>
              <div class="footer-col footer-col-2">
                <ul class="social-media-list">
                  {for (service, username) <- siteHtml.getSocial.list yield
                  <li>
                    <a href={s"${service.serviceUrl}/$username"}>
                      <svg class="svg-icon">
                        <use xmlns:xlink={XLink.namespace.uri} xlink:href={service.iconUrl}/>
                      </svg>
                      <span class="username">
                        {username}
                      </span>
                    </a>
                  </li>}
                </ul>
              </div>{ScalaXml.optional(siteHtml.footer)(footer =>
              <div class="footer-col footer-col-3">
                {footer.content.scalaXml}
              </div>)}
            </div>
          </div>
        </footer>
      </body>
      <script type='module'>
        import loadWindow from '/js/window.js';
        loadWindow({Json.stringToJs(viewer)}, {Json.optionToJs(siteHtml.googleAnalyticsId)});
      </script>
      {ScalaXml.conditional(isMathJaxEnabled    )(math                   .body)}
      {ScalaXml.conditional(isHighlighterEnabled)(siteHtml.getHighlighter.body)}
    </html>

  private val pageLinkClass: Attribute.Value[String] = Html.classAttribute.required.withValue("page-link")

  // TODO implement and move into mathjax and highlighter correspondingly:
  private def isMathPresent(content: ScalaXml.Element): Boolean = true // TODO
  private def isCodePresent(content: ScalaXml.Element): Boolean = true

  private def tooltip(content: ScalaXml.Nodes): ScalaXml.Element =
    <span xmlns={namespace.uri} class="tooltip">
      {content}
    </span>
  
  def addTooltip(content: ScalaXml.Nodes, element: ScalaXml.Element): ScalaXml.Element =
    ScalaXml.prependChildren(element, tooltip(content))

  def footnote(contentId: String, srcId: String, symbol: String, content: ScalaXml.Nodes): ScalaXml.Element =
    <span xmlns={namespace.uri} class="footnote" id={contentId}>
      <a href={s"#$srcId"} class="footnote-backlink">
        {symbol}
      </a>{content}
    </span>

  def footnoteRef(contentId: String, srcId: String, symbol: String): ScalaXml.Element =
    <a xmlns={namespace.uri} href={s"#$contentId"} class="footnote-link" id={srcId}>
      {symbol}
    </a>

  def footnoteLevel(content: Seq[ScalaXml.Element], depth: Int): ScalaXml.Nodes =
    <hr class="footnotes-line"/> ++
    <div xmlns={namespace.uri} class="footnotes">
      {content}
    </div>

  def table(children: ScalaXml.Nodes): ScalaXml.Element =
    <table xmlns={namespace.uri}>{children}</table>

  def tr(children: ScalaXml.Nodes): ScalaXml.Element =
    <tr xmlns={namespace.uri}>{children}</tr>

  def td(colspan: Option[String], children: ScalaXml.Nodes): ScalaXml.Element =
    <td xmlns={namespace.uri} colspan={colspan.orNull}>{children}</td>
