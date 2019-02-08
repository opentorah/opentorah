<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/html/chunk.xsl"/>

    <xsl:import href="common-html.xsl"/>

    <xsl:template name="user.head.content">
        <script type="text/javascript">
          window.MathJax = {
            MathML: {
              extensions: [ "content-mathml.js", "mml3.js" ]
            }
          };
        </script>
        <script type="text/javascript" src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML"/>
    </xsl:template>
</xsl:stylesheet>
