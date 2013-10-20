<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="/">
    <html>
      <head>
        <title>ProUnit Execution Report</title>
        <link href="styles_prounitdefault.css" rel="stylesheet" type="text/css" />
        <script type="text/javascript">
           function selectDiv(divId) {
               var element = document.getElementById("div_" + divId);

               try {
                 document.currentDIV.style.display = "none";
               } catch(e) {}
               element.style.display = "inline";
               document.currentDIV   = element;
               return;
           }

           function swapVisible(setName) {
               var element = document.getElementById(setName);
               var currentState = element.style.display;

               if(currentState == "" || currentState == "block")
                 element.style.display = "none";
               else
                 element.style.display = "block";
               return;
           }
        </script>
      </head>
      <body>
        <table border="0" height="100%" width="100%" cellpadding="0" cellspacing="0">
          <!-- title -->
          <tr height="30"><td align="center" class="ReportTitle">ProUnit Execution Report</td></tr>

          <!-- general information -->
          <tr height="30">
            <td>
              <table border="0" width="100%">
                <xsl:for-each select="/ProUnitExecutionLog">
                  <tr>
                    <td width="16%"></td>
                    <td width="17%" class="ReportLabel" align="right">Execution Timestamp:</td>
                    <td width="17%" class="ReportValue"><xsl:value-of select="@timestamp" /></td>
                    <td width="17%" class="ReportLabel" align="right">Final Status:</td>
                    <td width="17%" class="ReportValue"><xsl:value-of select="@status" /></td>
                    <td width="16%"></td>
                  </tr>
                </xsl:for-each>
              </table>
            </td>
          </tr>

          <!-- separatot -->
          <tr height="10"><td class="TransparentCell"></td></tr>

          <!-- detailed results -->
          <tr>
            <td>
              <table border="0" height="100%" width="100%" cellpadding="0" cellspacing="0">
                <tr>
                  <td id="treeview" valign="top" width="250">
                    <div width="20" style="width: 250px; height: 100%; overflow: scroll;">
                      <table>
                        <tr><td colspan="2" class="TreeTitle">ProUnit Test Framework</td></tr>
                        <tr>
                          <td width="10" />
                          <td>
                            <table>
                              <xsl:for-each select="/ProUnitExecutionLog">
                                <tr><td><xsl:apply-templates mode="treeview" /></td></tr>
                              </xsl:for-each>
                            </table>
                          </td>
                        </tr>
                      </table>
                    </div>
                  </td>

                  <td class="TransparentCell" width="10"></td>

                  <td id="result" valign="top">
                    <div id="DIV_0" class="ReportValue" align="center">
                      <br />
                      Check execution details by clicking on the Treeview Items.
                    </div>
                    <script type="text/javascript">
                      document.currentDIV = document.getElementById("DIV_0");
                    </script>
                    <xsl:for-each select="/ProUnitExecutionLog">
                      <xsl:apply-templates mode="results" />
                    </xsl:for-each>
                  </td>
                </tr>
              </table>
            </td>
          </tr>
        </table>

      </body>
    </html>
  </xsl:template>

  <!-- ********************************************************* -->
  <!-- Results templates                                         -->
  <!-- ********************************************************* -->
  <xsl:template match="TestSet" mode="results">
    <xsl:apply-templates mode="results" />
  </xsl:template>

  <xsl:template match="TestCase" mode="results">
    <div style="display: none;">
      <xsl:attribute name="id">div_<xsl:value-of select="@seq" /></xsl:attribute>
      <table border="0" width="100%" cellspacing="0" style="border-collapse: collapse;">
        <tr class="ResultHeader">
          <td align="center" class="ResultCell" width="150">Name</td>
          <td align="center" class="ResultCell" width="80">Status</td>
          <td align="center" class="ResultCell" width="80">First Assert Failed</td>
          <td align="center" class="ResultCell" width="80">Total Time</td>
          <td align="center" class="ResultCell">Message</td>
        </tr>
        <xsl:apply-templates mode="results" />
      </table>
    </div>
  </xsl:template>

  <xsl:template match="Test" mode="results">
    <!-- Unit tests report -->
    <tr>
      <xsl:attribute name="class"><xsl:value-of select="@status" /></xsl:attribute>
      <td class="ResultCell"><xsl:value-of select="@name" /></td>
      <td class="ResultCell"><xsl:value-of select="@status" /></td>
      <td class="ResultCell"><xsl:value-of select="@FirstAssertFailed" /></td>
      <td class="ResultCell"><xsl:value-of select="@TotalTime" /></td>
      <td class="ResultCell"><xsl:value-of select="@message" /></td>
    </tr>
  </xsl:template>


  <!-- ********************************************************* -->
  <!-- TreeView templates                                         -->
  <!-- ********************************************************* -->
  <xsl:template match="TestSet" mode="treeview">
    <table border="0">
      <tr>
        <td colspan="2">
          <a>
            <xsl:attribute name="href">javascript:swapVisible("set" + <xsl:value-of select="@seq" />)</xsl:attribute>
            <xsl:attribute name="class">Tree<xsl:value-of select="@status" /></xsl:attribute>
            <xsl:value-of select="@name" />
          </a>
        </td>
      </tr>
      <tr>
        <xsl:attribute name="id">set<xsl:value-of select="@seq" /></xsl:attribute>
        <td width="10" />
        <td>
          <table border="0" cellpadding="0" cellspacing="0">
            <tr><td><xsl:apply-templates mode="treeview"/></td></tr>
          </table>
        </td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="TestCase" mode="treeview">
    <table border="0" cellspacing="0">
      <tr>
        <td>
          <xsl:attribute name="class">Tree<xsl:value-of select="@status" /></xsl:attribute>
          <a>
            <xsl:attribute name="class">Tree<xsl:value-of select="@status" /></xsl:attribute>
            <xsl:attribute name="href">javascript:selectDiv(<xsl:value-of select="@seq" />)</xsl:attribute>
            <xsl:value-of select="@name" />
          </a>
        </td>
      </tr>
    </table>
  </xsl:template>

</xsl:stylesheet>
