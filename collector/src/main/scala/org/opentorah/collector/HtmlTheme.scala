package org.opentorah.collector

import org.opentorah.html.Html
import org.opentorah.xml.{Attribute, XLink, Xml}

object HtmlTheme {

  // TODO remove post-related stuff
  def toHtml(
    viewer: Viewer,
    headTitle: Option[String],
    title: Option[Xml.Nodes],
    style: String,
    favicon: String,
    googleAnalyticsId: Option[String],
    content: Xml.Element,
    header: Xml.Element,
    footer: Xml.Element
  ): Xml.Element =
    <html>
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        {headTitle.toSeq.map(title => <title>{title}</title>)}
        <link rel="stylesheet" href={s"/assets/$style.css"}/>
        <link rel="icon" type="image/jpeg" href={s"/$favicon.jpg"}/>
      </head>
      <body>
        {header}
        <main class="page-content" aria-label="Content">
          <div class="wrapper">
            <article class="post">
              {title.toSeq.map(title => <header class="post-header"><h1 class="post-title">{title}</h1></header>)}
              <div class="post-content">
                {content}
              </div>
            </article>
          </div>
        </main>
        {footer}
      </body>
      <script type='module'>
        import loadWindow from '/js/window.js';
        loadWindow('{viewer.name}', {optionToJs(googleAnalyticsId)});</script>
    </html>

  private def optionToJs(value: Option[String]): String =
    value.fold("null")(value => s"'$value'")

  private val pageLinkClass: Attribute.Value[String] = Html.classAttribute.required.withValue("page-link")

  def header(
    title: Xml.Nodes,
    navigationLinks: Seq[Xml.Element] // normally, <a>s
  ): Xml.Element =
    <header class="site-header" role="banner">
      <div class="wrapper">
        <a class="site-title" rel="author" target={Viewer.default.name} href="/">{title}</a>
        <nav class="site-nav">
          <div class="trigger">{navigationLinks.map(pageLinkClass.set)}</div>
        </nav>
      </div>
    </header>

  def footer(
    author: String,
    email: String,
    githubUsername: Option[String],
    twitterUsername: Option[String],
    footerCol3: Xml.Nodes
  ): Xml.Element =
    <footer class="site-footer h-card">
      <data class="u-url" href="/"/>

      <div class="wrapper">
        <div class="footer-col-wrapper">
          <div class="footer-col footer-col-1">
            <ul class="contact-list">
              <li class="p-name">{author}</li>
              <li><a class="u-email" href={s"mailto:$email"}>{email}</a></li>
            </ul>
          </div>
          <div class="footer-col footer-col-2">
            <ul class="social-media-list">{
              social(githubUsername, "github.com", "github") ++
              social(twitterUsername, "www.twitter.com", "twitter")
            }</ul>
          </div>
          <div class="footer-col footer-col-3">{footerCol3}</div>
        </div>
      </div>
    </footer>

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
