<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:k="http://www.tavultesoft.com/xml/70">

  <xsl:template name="menuframe_style">
    .menuframe { width: 100%; display: block; text-align: center; font: 9pt Tahoma; padding: 8px 0px 8px 2px; border-right: 2px solid #888888; background:none }
    .menuframe_active { width: 100%; display: block; text-align: center; font: 9pt Tahoma; padding: 6px 2px 6px 0px;
         background: white; border-top: 2px solid #888888; border-bottom: 2px solid #888888; border-left: 2px solid #888888; }
  </xsl:template>
  
  <xsl:template name="menuframe">
    <xsl:text disable-output-escaping="yes"><![CDATA[
      <script type="text/javascript">
          var menuframe_activeindex=0, menuframe_items = [];
          
          function menuframe_add(name, description, hotkey)
          {
              var n = menuframe_items.length;
              document.write('<span unselectable="on" class="menuframe" id="menuframe'+n+'"><img unselectable="on" alt="'+description+'" src="img/menuframe_'+name+'.gif" width="48" height="32" /><br />'+description+'</span>');
              menuframe_items.push(document.getElementById('menuframe'+n));
              menuframe_items[n].onmousedown = function() { menuframe_activate(n); }
              menuframe_items[n].menu_description = description;
              menuframe_items[n].menu_hotkey = hotkey;
              menuframe_items[n].menu_name = name;
          }
          
          function menuframe_activate(n)
          {
              var p = menuframe_items[menuframe_activeindex], c = menuframe_items[n];
              p.className='menuframe'; 
              c.className='menuframe_active'; 
              menuframe_activeindex=n;
              document.getElementById('content_'+p.menu_name).style.display = 'none';
              document.getElementById('content_'+c.menu_name).style.display = 'block';
          }
          
          function menuframe_keydown()
          {
              switch(event.keyCode)
              {
                case 40: /* down */ if(menuframe_activeindex < menuframe_items.length - 1) { menuframe_activate(menuframe_activeindex+1); } break;
                case 38: /* up */   if(menuframe_activeindex > 0) { menuframe_activate(menuframe_activeindex-1); } break;
                case 33: /* pgup */
                case 36: /* home */ menuframe_activate(0); break;
                case 34: /* pgdn */
                case 35: /* end */  menuframe_activate(menuframe_items.length - 1); break;
                default: 
                  for(var i = 0; i < menuframe_items.length; i++)
                      if(menuframe_items[i].menu_hotkey.charCodeAt(0) == event.keyCode)
                      {
                          menuframe_activate(i); break;
                      }
              }
              
          }

          function menuframe_setup()
          {
            var menuframe = document.getElementById('menuframe');
            menuframe.onkeydown = menuframe_keydown;
            menuframe.tabIndex = '1';
            menuframe_activate(0);
          }
          window.attachEvent('onload', menuframe_setup);
      </script>
      <script type="text/javascript">
          menuframe_add('keyboardlist', 'Keyboards', 'K');
          menuframe_add('options', 'Options', 'O');
          menuframe_add('hotkeys', 'Hotkeys', 'H');
          menuframe_add('addins', 'Addins', 'A');
          menuframe_add('languages', 'Languages', 'L');
          menuframe_add('support', 'Support', 'S');
      </script>
    ]]></xsl:text>
    <span class="menuframe" style="height: 100%"><xsl:text></xsl:text></span>
  </xsl:template>

</xsl:stylesheet>
