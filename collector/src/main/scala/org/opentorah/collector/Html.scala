package org.opentorah.collector

import scala.xml.Elem

object Html {

  // TODO add  <!DOCTYPE html>
  // TODO relativize URLs in general and u-url in particular?
  // TODO escape URLs?
  // TODO abstract over window/viewer names.

  // Note: meta and link elements are closed to make Scala XML happy; I think this should work in the browsers :)
  // Skipped {seo} after metas and feedMeta before googleAnalytics - I do not see them in Minima sources *and*
  // I do not use them :)
  def defaultLayout(
    siteParameters: SiteParameters,
    pageParameters: PageParameters,
    content: Elem
  ): Elem =
    <html lang={pageParameters.lang}>
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <link rel="stylesheet" href={s"/assets/${pageParameters.style}.css"}/>
        {siteParameters.googleAnalyticsId.fold[Seq[Elem]](Seq.empty){ googleAnalyticsId =>
          Seq(<script>{googleAnalytics(googleAnalyticsId)}</script>)}}
        <link rel="icon" type="image/jpeg" href={s"/${siteParameters.faviconJpeg}.jpeg"}/>
      </head>
      <body>
        {header(siteParameters, pageParameters)}
        <main class="page-content" aria-label="Content">
          <div class="wrapper">{content}</div>
        </main>
        {footer(siteParameters)}
      </body>
      {pageParameters.target.fold[Seq[Elem]](Seq.empty){ target =>
        Seq(<script type="module">{s"""window.name = "$target"""}</script>)}}
    </html>

  // Only in production :)
  def googleAnalytics(id: String): String =
    s"""
       |if(!(window.doNotTrack === "1" || navigator.doNotTrack === "1" || navigator.doNotTrack === "yes" || navigator.msDoNotTrack === "1")) {
       |  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
       |  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
       |  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
       |  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
       |
       |  ga('create', '$id', 'auto');
       |  ga('send', 'pageview');
       |}
       |""".stripMargin

  def header(siteParameters: SiteParameters, pageParameters: PageParameters): Elem =
    <header class="site-header" role="banner">
      <div class="wrapper">
        <a class="site-title" rel="author" target="collectionViewer" href="/">{siteParameters.title}</a>
        <nav class="site-nav">
          <div class="trigger">{
            for (link <- siteParameters.navigationLinks ++ pageParameters.navigationLinks)
            yield <a class="page-link" href={link.url} target={link.target.orNull}>{link.title}</a>
          }</div>
        </nav>
      </div>
    </header>

  // TODO doesn't seem to work - fix the CSS or remove?
  // To enable, add {navTrigger} <nav class="site-nav">
  val navTrigger: Seq[Elem] = Seq(
      <input type="checkbox" id="nav-trigger" class="nav-trigger"/>,
      <label for="nav-trigger">
        <span class="menu-icon">
          <svg viewBox="0 0 18 15" width="18px" height="15px">
            <path d="M18,1.484c0,0.82-0.665,1.484-1.484,1.484H1.484C0.665,2.969,0,2.304,0,1.484l0,0C0,0.665,0.665,0,1.484,0 h15.032C17.335,0,18,0.665,18,1.484L18,1.484z M18,7.516C18,8.335,17.335,9,16.516,9H1.484C0.665,9,0,8.335,0,7.516l0,0 c0-0.82,0.665-1.484,1.484-1.484h15.032C17.335,6.031,18,6.696,18,7.516L18,7.516z M18,13.516C18,14.335,17.335,15,16.516,15H1.484 C0.665,15,0,14.335,0,13.516l0,0c0-0.82,0.665-1.483,1.484-1.483h15.032C17.335,12.031,18,12.695,18,13.516L18,13.516z"/>
          </svg>
        </span>
      </label>)

  def footer(siteParameters: SiteParameters): Elem =
    <footer class="site-footer h-card">
      <data class="u-url" href="/"/>

      <div class="wrapper">
        <div class="footer-col-wrapper">
          <div class="footer-col footer-col-1">
            <ul class="contact-list">
              <li class="p-name">{siteParameters.author}</li>
              <li><a class="u-email" href={s"mailto:${siteParameters.email}"}>{siteParameters.email}</a></li>
            </ul>
          </div>
          <div class="footer-col footer-col-2">{social}</div>
          <div class="footer-col footer-col-3">{siteParameters.footerCol3}</div>
        </div>
      </div>
    </footer>

  def social: Seq[Elem] = Seq.empty

  def pageLayout(
    siteParameters: SiteParameters,
    pageParameters: PageParameters,
    content: Elem
  ): Elem = defaultLayout(
    siteParameters,
    pageParameters,
    <article class="post">
      <header class="post-header">
        <h1 class="post-title">{pageParameters.title.get}</h1>
      </header>

      <div class="post-content">
        {content}
      </div>
    </article>
  )
}
