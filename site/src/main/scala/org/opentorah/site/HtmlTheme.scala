package org.opentorah.site

import org.opentorah.html.Html
import org.opentorah.mathjax.MathJax
import org.opentorah.xml.{Attribute, XLink, Xml}

object HtmlTheme {

  // TODO remove post-related stuff
  // TODO conditionalize syntax highlighting (https://highlightjs.org/)
  // TODO consolidate css, js etc. under 'asset'
  // TODO do not force the favicon to be jpeg
  def toHtml[S <: Site[S]](
    htmlContent: HtmlContent[S],
    navigationLinks: Seq[Xml.Element], // normally, <a>s
    content: Xml.Element,
    site: S
  ): Xml.Element =
    <html>
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        {htmlContent.htmlHeadTitle.toSeq.map(title => <title>{title}</title>)}
        <link rel="stylesheet" href={s"/css/${site.style(htmlContent)}.css"}/>
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css" integrity="sha384-wvfXpqpZZVQGK6TAh5PVlGOfQNHSoD2xbE+QkPxCAFlNEevoEH3Sl0sibVcOQVnN" crossorigin="anonymous"/>
        <link rel="icon" href={s"/${site.favicon}"}/>
        <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.7.2/build/styles/default.min.css"/>
        <script src="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.7.2/build/highlight.min.js"></script>
        <script>hljs.highlightAll();</script>
        {if (!site.isMathJaxEnabled) Seq.empty else {
           val mathJax: MathJax = MathJax.get(site.useMathJax3)
           mathJax.head(Xml.mkText(mathJax.htmlConfigurationString(site.mathJaxConfiguration)))}}
      </head>
      <body>
        <header class="site-header" role="banner">
          <div class="wrapper">
            <a class="site-title" rel="author" target={site.defaultViewer.name} href="/">{site.title.xml}</a>
            <nav class="site-nav">
              <input type="checkbox" id="nav-trigger" class="nav-trigger" />
              <label for="nav-trigger">
                <span class="menu-icon">
                  <svg viewBox="0 0 18 15" width="18px" height="15px">
                    <path d="M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z"/>
                  </svg>
                </span>
              </label>
              <div class="trigger">{navigationLinks.map(pageLinkClass.set)}</div>
            </nav>
          </div>
        </header>
        <main class="page-content" aria-label="Content">
          <div class="wrapper">
            <article class="post">
              {htmlContent.htmlBodyTitle.toSeq.map(title => <header class="post-header"><h1 class="post-title">{title}</h1></header>)}
              <div class="post-content">
                {content}
              </div>
            </article>
          </div>
        </main>
        <footer class="site-footer h-card">
          <data class="u-url" href="/"/>
          <div class="wrapper">
            <div class="footer-col-wrapper">
              <div class="footer-col footer-col-1">
                <ul class="contact-list">
                  <li class="p-name">{site.siteUrl}</li>
                  <li><a class="u-email" href={s"mailto:${site.email}"}>{site.email}</a></li>
                </ul>
              </div>
              <div class="footer-col footer-col-2">
                <ul class="social-media-list">{
                  social(site.githubUsername, "github.com", "github") ++
                  social(site.twitterUsername, "www.twitter.com", "twitter")
                }</ul>
              </div>
              <div class="footer-col footer-col-3">{site.footer.xml}</div>
            </div>
          </div>
        </footer>
      </body>
      <script type='module'>
        import loadWindow from '/js/window.js';
        loadWindow('{site.viewer(htmlContent).name}', {optionToJs(site.googleAnalyticsId)});</script>
    </html>

  private def optionToJs(value: Option[String]): String =
    value.fold("null")(value => s"'$value'")

  private val pageLinkClass: Attribute.Value[String] = Html.classAttribute.required.withValue("page-link")

  private def social(username: Option[String], serviceUrl: String, iconPart: String): Seq[Xml.Element] =
    username.toSeq.map(username =>
    <li>
      <a href={s"https://$serviceUrl/$username"}>
        <svg class="svg-icon">
          <use xmlns:xlink={XLink.namespace.uri} xlink:href={s"/assets/icons.svg#$iconPart"}/>
        </svg>
        <span class="username">{username}</span>
      </a>
    </li>)
}
