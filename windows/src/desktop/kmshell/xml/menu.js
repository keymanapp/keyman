var menudiv = null, global_menu_elem_name = null;

  function RightButtonMenu( menuid ) {
    if( (event.clientX + 180) > document.body.clientWidth ) {
      ShowMenu( menuid,'right', event.clientX, event.clientY );
    } else {
      ShowMenu( menuid,'left', event.clientX, event.clientY );
    }
  }

  function PageOffset(e)
  {
    var pt = {x: 0, y: 0};
    while(e)
    {
      pt.x += e.offsetLeft; pt.y += e.offsetTop; pt.x -= e.scrollLeft; pt.y -= e.scrollTop;
      e = e.offsetParent;
    }
    return pt;
  }

  function ShowMenu(name,align,xpos,ypos) {
    global_menu_elem_name = name;
    var q;
    var menu = document.getElementById('menu_'+name), button = document.getElementById('button_'+name);
    var index = name.substr(8,2);

    menudiv.innerHTML = menu.innerHTML;
    menudiv.style.width = menu.offsetWidth + 'px';

    (function(m) {
      for(var i = 0; i < m.children.length; i++) {
        if(m.children[i].id != '') { m.children[i].id = 'Popup_'+m.children[i].id; }
      }
    })(menudiv);

    var pb = PageOffset(button);
    var pm = PageOffset(menu.offsetParent);

    menudiv.style.zIndex=100;
    menudiv.style.visibility='visible';

    if( xpos && ypos ) {
      if(align=='right')
        menudiv.style.left = (xpos-menudiv.offsetWidth) + 'px';
      else
        menudiv.style.left = xpos + 'px';
      menudiv.style.top = ypos + 'px';
    } else {
      if(align=='right') menudiv.style.left = (pb.x+button.offsetWidth-menudiv.offsetWidth) + 'px';
        else menudiv.style.left = pb.x + 'px';
      menudiv.style.top = (pb.y+button.offsetHeight) + 'px';
    }


    q = menudiv.children[0];
    q.focus();

    document.getElementById('menubackground').className = 'show';
  }

  function HideMenu() {
    if(event.srcElement != menudiv) {
      global_menu_elem_name = null;
      menudiv.style.visibility='hidden';
      document.getElementById('menubackground').className = '';
      try {
        if(lastfocus_item != null) lastfocus_item.focus();
      } catch(e) {
        ; // I912
      }
    }
  }

  function menuitem_keydown(n)
  {
    switch(event.keyCode) {
      case 27:  // esc
        /* esc */
        HideMenu();
        event.cancelBubble = true;
        event.returnValue = false;
        return false;
      case 13:
      case 32:  // enter, spacebar
        HideMenu();
        menuitemdown(true);
        event.cancelBubble = true;
        event.returnValue = false;
        return true;

    case 35:    // end
      var elem = event.srcElement.parentElement;
      var j = elem.children.length - 1;
      elem.children[j].focus();
      break;
    case 36:    // home
      var elem = event.srcElement.parentElement;
      for( var i = 0; i < elem.children.length; i++ ) {
        if( elem.children[i].tabIndex ) {
          elem.children[i].focus();
          break;
        }

      }
      break;
    case 38:    // up
      // avoid dropdowns
      if( event.srcElement.tagName.toLowerCase() == 'select' ) {
        event.cancelBubble=true;
        return true;
      }

      // find prev list element
      var elem = event.srcElement.parentElement;
      for(var i = 0; i < elem.children.length; i++) {
        if(elem.children[i] == event.srcElement)
        {
          j = i;
          do {
            if(--j < 0) j = elem.children.length-1;
            if(elem.children[j].tabIndex)
            {
              elem.children[j].focus();
              break;
            }
          } while(j != i);
          break;
        }
      }
      break;
    case 40:    // down
      // avoid dropdowns
      if( event.srcElement.tagName.toLowerCase() == 'select' ) {
        event.cancelBubble=true;
        return true;
      }

      // find next file element
      var elem = event.srcElement.parentElement;
      for(var i = 0; i < elem.children.length; i++) {
        if(elem.children[i] == event.srcElement)
        {
          j = i;
          do {
            if(++j >= elem.children.length) j = 0;
            if(elem.children[j].tabIndex)
            {
              elem.children[j].focus();
              break;
            }
          } while(j != i);
          break;
        }
      }
      break;
    default:
      event.returnValue = true;
      return true;
    }
    event.returnValue = false;
    event.cancelBubble = true;
    return false;
  }

  function menuitemover()
  {
    var e = event.srcElement; while(e && e.tagName.toLowerCase() != 'span') e=e.parentElement;
    if(e) { e.className="menuitem hover"; e.focus(); } // I1601
  }
  function menuitemout()
  {
    var e = event.srcElement; while(e && e.tagName.toLowerCase() != 'span') e=e.parentElement;
    if(e) e.className="menuitem";
  }
  function menuitemdown(b)
  {
    if(b) {
      var e = event.srcElement;
      if (e) {
        location.href = e.attributes['command'].value;
        e.className="menuitem";
      }
    } else {
      event.returnValue = false;
      event.cancelBubble = true;
    }
  }

  function menu_doAttachEvent(obj, e, f)
  {
    if(obj.attachEvent) return obj.attachEvent('on'+e, f);
    return obj.addEventListener(e, f, false);
  }

  function menusetup() {
    menudiv = document.createElement('SPAN');
    menudiv.className='menu';
    menu_doAttachEvent(menudiv, 'mouseup', HideMenu);
    document.body.appendChild(menudiv);
  }
  menu_doAttachEvent(window, 'load', menusetup);

