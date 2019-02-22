<?xml version="1.0" encoding="UTF-8"?>
<!-- Customizations go here. -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
                xmlns:db="http://docbook.org/ns/docbook" exclude-result-prefixes="db">

  <!-- Number everything arabically except for sections. -->
  <xsl:param name="autolabel.elements">
    <db:appendix format="A"/>
    <db:chapter/>
    <db:figure/>
    <db:example/>
    <db:table/>
    <db:equation/>
    <db:part format="I"/>
    <db:reference format="I"/>
    <db:preface/>
    <db:qandadiv/>
    <db:section/>
    <db:refsection/>
  </xsl:param>

  <!-- Add MathJax support -->
  <xsl:template name="user.head.content">
    <script type="text/javascript">
      window.MathJax = { MathML: { extensions: [ "content-mathml.js", "mml3.js" ] } };
    </script>
    <script type="text/javascript"
            src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML"/>
  </xsl:template>
</xsl:stylesheet>
