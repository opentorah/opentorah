<?xml version="1.0" encoding="UTF-8"?>
<!-- Customizations go here. -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <!-- Add MathJax support -->
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
