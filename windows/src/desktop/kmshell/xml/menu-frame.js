  var menuframe_activeindex=0, menuframe_items = [];

  function menuframe_add(path, name, description, hotkey)
  {
    var n = menuframe_items.length;
    document.write('<span unselectable="on" class="menuframe" id="menuframe'+n+'"><span class="menuframe_'+name+'">' +
      '<img unselectable="on" alt="'+description+'"'+'src="'+path+
      'menuframe_'+name+'.png" width="114" height="32" /><br />'+description+'</span></span>');
    menuframe_items.push(document.getElementById('menuframe'+n));
    document.getElementById('menuframe'+n).onclick = function(event) { menuframe_activate(n); }
    //menuframe_items[n].onclick = function() { menuframe_activate(n); }
    menuframe_items[n].menu_description = description;
    menuframe_items[n].menu_hotkey = hotkey;
    menuframe_items[n].menu_name = name;
  }

  function menuframe_activate(n)
  {
    var p = menuframe_items[menuframe_activeindex], c = menuframe_items[n];
    var itemtype, blah;

    p.className='menuframe';
    c.className='menuframe_active';
    menuframe_activeindex=n;
    document.getElementById('content_'+p.menu_name).style.display = 'none';
    document.getElementById('content_'+c.menu_name).style.display = 'block';

    save_state();

    if(!loading_state) {
      var q = document.getElementById('content_'+c.menu_name).getElementsByTagName('span');
      for(var i = 0; i < q.length; i++) {
        blah = q[i].id;
        itemtype = blah.substr(0, 5);

        if( itemtype == "list_" ) {
          q[i].focus();
          break;
        }
      }
    }

    if(typeof window['menuframe_activate_'+c.menu_name] != 'undefined') {
      window['menuframe_activate_'+c.menu_name]();
    }
  }

  function menuframe_keydown(event)
  //document.onkeydown = function()
  {
    if(!event) event = window.event;
    switch(event.keyCode) {
      case 9:
        if( event.ctrlKey == true && event.shiftKey == false ) {
          event.cancelBubble = true;
          event.returnValue = false;
          if(menuframe_activeindex < menuframe_items.length - 1) {
            menuframe_activate(menuframe_activeindex+1);
          }
        } else if( event.ctrlKey == true && event.shiftKey == true ) {
          event.cancelBubble = true;
          event.returnValue = false;
          if(menuframe_activeindex > 0) {
            menuframe_activate(menuframe_activeindex-1);
          }
        } else event.returnValue = true;
        break;
      case 34: /* pgdn */
        event.cancelBubble = true;
        event.returnValue = false;
        if(menuframe_activeindex < menuframe_items.length - 1) {
          menuframe_activate(menuframe_activeindex+1);
        }
        break;
      case 33: /* pgup */
        event.cancelBubble = true;
        event.returnValue = false;
        if(menuframe_activeindex > 0) {
          menuframe_activate(menuframe_activeindex-1);
        }
        break;
      case 13:    // enter
        event.cancelBubble = true;
        event.returnValue = event.target.id.substring(0,6) == 'button' || event.target.tagName == 'TEXTAREA';
        break;
      case 27:    // esc
        event.cancelBubble = true; event.returnValue = false;
        break;
      case 32:    // spacebar
        switch(event.srcElement.tagName.toLowerCase()) {
          case 'select':
            var k = 'list_' + event.srcElement.index;
            document.getElementById(k).focus();
            break;
          case 'option':
            var k = 'list_' + event.srcElement.index;
            alert(k);
            document.getElementById(k).focus();
            break;
          case 'input': return true;
          case 'div':
          case 'span':
            if(event.srcElement.parentElement) {
              switch(event.srcElement.parentElement.id) {
                case 'keyboards':
                  keyboard_checkclick(event.srcElement.id.substring('list_keyboard_'.length),true);
                  break;
                case 'options':
                  options_updatecheck(event.srcElement.id.substring('list_option_'.length),true);
                  break;
                case 'subcontent_hotkeys':
                  location.href='keyman:hotkey_set?index=hotkey_' + event.srcElement.id.substring('list_hotkey_'.length);
                  break;
                default:
                  break;
              }
            }
            break;
        }
        event.cancelBubble = true; event.returnValue = false; event.preventDefault(); break;
        break;
      default:
        for(var i = 0; i < menuframe_items.length; i++)
          if(menuframe_items[i].menu_hotkey.charCodeAt(0) == event.keyCode && event.altKey)
          {
              event.cancelBubble = true; event.returnValue = false; menuframe_activate(i); break;
          }

    }
  }

  function menuframe_setup() {
    document.onkeydown = menuframe_keydown;
  }
  doAttachEvent(window, 'load', menuframe_setup);

