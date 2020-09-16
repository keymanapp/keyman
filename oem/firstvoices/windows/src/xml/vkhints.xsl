<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:include href="elements.xsl"/>

	<xsl:template match="/">

		<html>
			<head>
				<!--<META http-equiv="Page-Enter" CONTENT="progid:DXImageTransform.Microsoft.Pixelate(Duration=1)" />-->

				<title>hintbar</title>
				<style type="text/css">
          * { font-family: Tahoma; font-size: 11px; }

          body { padding: 0px 0px 0px 0px; margin: 0px; overflow: hidden;  width: 100%; height: 100%; background: #5E97A8 ; }
          html { padding: 0px; margin: 0px; overflow: hidden; }

          #size { width: 100%; height: 20px; position: absolute; top: -30px }

          #outer { height: 100%; width: 100%; position: absolute; left: 0; top: 0; overflow: hidden; }
          #center { position: relative; left: 0; top: 50%; width: 100%; height: 100% }
          #inner { position: relative; top: -50%; width: 100%; height: 100%; overflow: hidden; white-space: nowrap; padding: 2px 0px 0px 0px; }
          #control { padding-right: 2px; padding-left: 2px; position: absolute; top: -50%; right: -1px; padding-top: 2px; z-index: 1; background: #5E97A8; }

          a.button, a.button:visited { padding: 2px; display: block; float: left; }
          a.button:hover { background: #A1BDD7; }
          a.button img { border: none }

          .hint { color: #222222; display: none; white-space: nowrap; position: relative; left: 0; top: 2px; height: 16px; vertical-align: middle; float: left; }
          .hint img { vertical-align: bottom; width:16px; height:16px; }
          #hinttitle { color: #222222; display: block; height: 16px; vertical-align: middle; padding: 0 4px 0 4px; background: #5E97A8; z-index: 2; position: relative; left: 0; top: 2px; float: left; }
        </style><script type="text/javascript"><![CDATA[
					var hints = new Array(), currhint = 0, activehint = null, scrollInterval = 0, startScrollTimeout = null, autohintInterval = null;
					function finishload()
					{
						var elems = document.getElementsByTagName('div');
						for(var i in elems)
						if(elems[i].className == 'hint') hints.push(elems[i]);
						nexthint();
					}
          
          function filterchange()
          {
            if(inner.filters[0].status == 0)
            {
              setScrollTimeout(2000);
              inner.style.filter='';
            }
          }
          
          function setScrollTimeout(tm)
          {
            if(startScrollTimeout != null) window.clearTimeout(startScrollTimeout);
            startScrollTimeout = window.setTimeout(startScroll, tm);
          }
          
          function startScroll()
          {
            stopScroll();
            if(!activehint) return;
            activehint.style.left = '0px';
            if(activehint.offsetWidth+activehint.offsetLeft > inner.offsetWidth - control.offsetWidth)
              scrollInterval = window.setInterval(scroll, 75);
          }
          
          function stopScroll()
          {
            if(scrollInterval != null) window.clearInterval(scrollInterval);
            scrollInterval = null;
          }

          function scroll()
          {
            if(!activehint) return;
            if(activehint.offsetWidth+activehint.offsetLeft > inner.offsetWidth - control.offsetWidth)
              activehint.style.left = (activehint.offsetLeft-hinttitle.offsetWidth-1) + 'px';
            else
            {
              stopScroll();
              setScrollTimeout(5000);
            }
          }
          
					function autohint()
					{
            inner.style.filter='progid:DXImageTransform.Microsoft.Fade(duration=0.25,overlap=1.0)';
						inner.filters[0].Apply();
            gotohint(1);
						inner.filters[0].Play();
          }
          
          function gotohint(n)
          {
						if(hints.length == 0) return;
						
            stopScroll();
            
						hints[currhint].style.display = '';
            hints[currhint].style.left = '0px';
            currhint += n;
						if(currhint >= hints.length) currhint = 0;
            else if (currhint < 0) currhint = hints.length - 1;
						hints[currhint].style.display = 'block';
            activehint = hints[currhint];
					}

          function nexthint()
					{
            inner.style.filter='';
            gotohint(1);
            if(autohintInterval != null) window.clearInterval(autohintInterval);
            autohintInterval = window.setInterval(autohint, 5000);
            setScrollTimeout(2000);
					}

					function prevhint()
					{
            inner.style.filter='';
						gotohint(-1);
            if(autohintInterval != null) window.clearInterval(autohintInterval);
            autohintInterval = window.setInterval(autohint, 5000);
            setScrollTimeout(2000);
					}
				]]></script>
			</head>

			<body onload="finishload()">
				<div id="size"></div>
				<div id="outer">
					<div id="center">
						<div id="control">
							<a class="button" href="javascript:prevhint();">
								<img style="height:13px; width:13px">
									<xsl:attribute name="src">
										<xsl:value-of select="/Keyman/templatepath"/>hint_prev.gif
									</xsl:attribute>
								</img>
							</a>
							<a class="button" href="javascript:nexthint();">
								<img style="height:13px; width:13px">
									<xsl:attribute name="src">
										<xsl:value-of select="/Keyman/templatepath"/>hint_next.gif
									</xsl:attribute>
								</img>
							</a>
              <a class="button" href="javascript:location.href='keyman:closehintbar'">
                <img style="height:13px; width:13px"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>hint_close.gif</xsl:attribute></img>
              </a>
						</div>

						<div id="inner" onfilterchange="filterchange()">
							<div id="hinttitle"><b><xsl:value-of select="$locale/string[@name='S_OSK_Hint_Title']"/></b></div>
              <div class="hint"><xsl:value-of select="$locale/string[@name='S_OSK_Hint1']" /></div>
              <div class="hint"><xsl:value-of select="$locale/string[@name='S_OSK_Hint2']" /></div>
              <div class="hint">
                <xsl:value-of select="$locale/string[@name='S_OSK_Hint3a']" />
                <xsl:text xml:space="preserve"> </xsl:text>
                <img alt="icon"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>charmapicon.gif</xsl:attribute></img>
                <xsl:text xml:space="preserve"> </xsl:text>
                <xsl:value-of select="$locale/string[@name='S_OSK_Hint3b']" />
              </div>
              <div class="hint">
                <xsl:value-of select="$locale/string[@name='S_OSK_Hint4a']" />
                <xsl:text xml:space="preserve"> </xsl:text>
                <img alt="icon"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>fonthinticon.gif</xsl:attribute></img>
                <xsl:text xml:space="preserve"> </xsl:text>
                <xsl:value-of select="$locale/string[@name='S_OSK_Hint4b']" />
              </div>
              <div class="hint">
                <xsl:value-of select="$locale/string[@name='S_OSK_Hint5a']" />
                <xsl:text xml:space="preserve"> </xsl:text>
                <img alt="icon"><xsl:attribute name="src"><xsl:value-of select="/Keyman/templatepath"/>keyboardhelpicon.gif</xsl:attribute></img>
                <xsl:text xml:space="preserve"> </xsl:text>
                <xsl:value-of select="$locale/string[@name='S_OSK_Hint5b']" />
              </div>
						</div>
					</div>
				</div>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>
