package org.opentorah.site

import org.opentorah.html.Html
import org.opentorah.util.Json
import org.opentorah.xml.{Attribute, ScalaXml, XLink}

object HtmlTheme:

  // TODO put scripts on the bottom of body, libraries in the head?
  // TODO copy SEO stuff from Minima...
  // TODO remove post-related stuff
  // TODO conditionalize (and update) the tag character
  // TODO consolidate css, js etc. under 'asset'
  def toHtml[S <: Site[S]](
    htmlContent: HtmlContent[S],
    navigationLinks: Seq[ScalaXml.Element], // normally, <a>s
    content: ScalaXml.Element,
    site: S
  ): ScalaXml.Element =
    val common: SiteCommon = site.common
    val isMathJaxEnabled    : Boolean = common.getMathJax    .isEnabled && HtmlTheme.isMathPresent(content)
    val isHighlighterEnabled: Boolean = common.getHighlighter.isEnabled && HtmlTheme.isCodePresent(content)

    <html>
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        {ScalaXml.optional(htmlContent.htmlHeadTitle)(title => <title>{title}</title>)}
        <link rel="stylesheet" href={s"/css/${htmlContent.style}.css"}/>
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css" integrity="sha384-wvfXpqpZZVQGK6TAh5PVlGOfQNHSoD2xbE+QkPxCAFlNEevoEH3Sl0sibVcOQVnN" crossorigin="anonymous"/>
        {ScalaXml.optional(common.favicon)(favicon => <link rel="icon" href={s"/$favicon"}/>)}
        {if !isHighlighterEnabled then Seq.empty else common.getHighlighter.head}
      </head>
      <body>
        {if !isMathJaxEnabled     then Seq.empty else common.getMathJax    .body}
        {if !isHighlighterEnabled then Seq.empty else common.getHighlighter.body}
        <header class="site-header" role="banner">
          <div class="wrapper">
            {ScalaXml.optional(common.title)(title => <a class="site-title" rel="author"
               target={HtmlContent.hierarchyViewer} href="/">{title.content.scalaXml}</a>)}
            <nav class="site-nav">
              <input type="checkbox" id="nav-trigger" class="nav-trigger" />
              <label for="nav-trigger">
                <span class="menu-icon">
                  <svg viewBox="0 0 18 15" width="18px" height="15px">
                    <path d="M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z"/>
                  </svg>
                </span>
              </label>
              <div class="trigger">{navigationLinks.map(pageLinkClass.set(ScalaXml))}</div>
            </nav>
          </div>
        </header>
        <main class="page-content" aria-label="Content">
          <div class="wrapper">
            <article class="post">
              {ScalaXml.optional(htmlContent.htmlBodyTitle)(title => <header class="post-header"><h1 class="post-title">{title}</h1></header>)}
              <div class="post-content">{content}</div>
            </article>
          </div>
        </main>
        <footer class="site-footer h-card">
          <data class="u-url" href="/"/>
          <div class="wrapper">
            <div class="footer-col-wrapper">
              <div class="footer-col footer-col-1">
                <ul class="contact-list">
                  {ScalaXml.optional(common.url)(url => <li class="p-name">{url}</li>)}
                  {ScalaXml.optional(common.email)(email => <li><a class="u-email" href={s"mailto:$email"}>{email}</a></li>)}
                </ul>
              </div>
              <div class="footer-col footer-col-2">
                <ul class="social-media-list">{
                  for (service, username) <- common.getSocial.list yield
                    <li>
                      <a href={s"${service.serviceUrl}/$username"}>
                        <svg class="svg-icon">
                          <use xmlns:xlink={XLink.namespace.uri} xlink:href={service.iconUrl}/>
                        </svg>
                        <span class="username">{username}</span>
                      </a>
                    </li>
                }</ul>
              </div>
              {ScalaXml.optional(common.footer)(footer => <div class="footer-col footer-col-3">{footer.content.scalaXml}</div>)}
            </div>
          </div>
        </footer>
      </body>
      <script type='module'>
        import loadWindow from '/js/window.js';
        loadWindow({Json.stringToJs(htmlContent.viewer)}, {Json.optionToJs(common.googleAnalyticsId)});</script>
    </html>

  private val pageLinkClass: Attribute.Value[String] = Html.classAttribute.required.withValue("page-link")

  private def isMathPresent(content: ScalaXml.Element): Boolean = true // TODO
  private def isCodePresent(content: ScalaXml.Element): Boolean = true // TODO
