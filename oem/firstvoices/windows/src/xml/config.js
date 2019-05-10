var menudiv = null, global_menu_elem_name = null;
var globalfocus = null, globalfocus_item = null, lastfocus_item = null;

function doAttachEvent(obj, e, f)
{
  if(obj.attachEvent) return obj.attachEvent('on'+e, f);
  return obj.addEventListener(e, f, false);
}

function $(e)
{
  return document.getElementById(e);
}

function windowResize()
{
  var 
    eContent = $('contentframe'), 
    eFooter = $('footerframe'), 
    eMenu = $('menuframe'),
    eTitle = $('titleframe');
    
  if(eContent && eFooter && eMenu)
  {
    eContent.style.height = (document.body.offsetHeight - eFooter.offsetHeight - eTitle.offsetHeight) + 'px';
    eContent.style.width = (document.body.offsetWidth - eMenu.offsetWidth) + 'px';
    eContent.style.left = eMenu.offsetWidth + 'px';
    eContent.style.top = eTitle.offsetHeight + 'px';
    eMenu.style.height = (document.body.offsetHeight - eFooter.offsetHeight - eMenu.offsetTop) + 'px';
    
    var h = (eContent.offsetHeight - 37 - 2) + 'px'; // header height and border height
    $('subcontent_keyboards').style.height = h;
    $('subcontent_options').style.height = h;
    $('subcontent_hotkeys').style.height = h;
    $('subcontent_support').style.height = h;
  }
}

doAttachEvent(window, 'resize', windowResize);
doAttachEvent(window, 'load', windowResize);

  /*function pagesetup()
  {
    for(var i = 0; i<document.all.length; i++) {
      document.all[i].unselectable = 'on';
      document.all[i].style.cursor = 'default';
    }
  }
  
  window.attachEvent('onload', pagesetup);*/
  
  function list_mousedown(event,n)
  {
    var e = document.getElementById('list_'+n);
    window.setTimeout(function() { e.focus(); }, 10);
  }
  
  function list_focus(event,n)
  {
    return true;
  }
  
  function list_blur(event,n)
  {
    return list_unhover(event,n);
  }
  
  function list_hover(event,n)
  {
    if( menudiv.style.visibility != 'visible' ) {
      // document.getElementById('list_'+n).style.background = (globalfocus==document.getElementById('list_'+n)?'#CEEDFB':'#ECF8FE');
      document.getElementById('list_'+n).className = 'list_item '+(globalfocus_item==document.getElementById('list_'+n)?'list_item_focus':'list_item_hover');
      //document.getElementById('list_'+n).style.cursor = "default";
    }
    event.cancelBubble=true; return true;
  }
  function list_unhover(event,n)
  {
    if( //(globalfocus == null || globalfocus.id != 'list_'+n) &&
        (menudiv.style.visibility != 'visible' || global_menu_elem_name != 'options_'+n) ) {
      document.getElementById('list_'+n).className = 
        globalfocus_item==document.getElementById('list_'+n) ? 'list_item list_item_focus' : 'list_item';
    }
    event.cancelBubble=true; return true;
  }
  
  function list_keydown(event,n)
  {
    switch(event.keyCode)
    {
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
    case 37:    // left
    case 109:   // minus
      var k = document.getElementById('list_detail_'+n);
      if( !k ) break;
      if(k.style.display == 'block') list_detail(event,n);
      break;
    case 39:    // right
    case 107:   // plus
      var k = document.getElementById('list_detail_'+n);
      if( !k ) break;
      if(!k.style.display || k.style.display == 'none') list_detail(event,n);
      break;
    case 93:    // context menu key
      if( document.getElementById('menu_options_'+n) != null ) {
        var x = document.getElementById('list_'+n).offsetLeft + document.getElementById('menuframe').offsetWidth + 100;
        var y = document.getElementById('list_'+n).offsetTop + document.getElementById('keyboards_header').offsetHeight + 10;
      
        ShowMenu( 'options_'+n, 'left', x, y);
      }
      event.cancelBubble = true;
      event.returnValue = false;
      return false;
    default:
      event.returnValue = true;
      return true;
    }
    event.returnValue = false;
    event.cancelBubble = true;
    return false;
  }

  document.onfocusin = function() {
    var itemtype, blah;
    
    globalfocus = event.srcElement;
    if(!event.srcElement) { return true; }
    elemid = event.srcElement.id;
    itemtype = elemid.substr(0, 5);
    
    if(event.srcElement.className.substr(0,8) != 'menuitem' && event.srcElement != menudiv
          && menudiv != null && menudiv.style.visibility=='visible') HideMenu();
    
    if(globalfocus_item != null) {
      if(globalfocus_item != event.srcElement && !globalfocus_item.contains(event.srcElement)) {
        /* globalfocus_item.style.background = '';
        globalfocus_item.style.filter = ''; */
        globalfocus_item = null;
      }        
    }
    if( event.srcElement.className.indexOf('list_item') >= 0 ) {
      globalfocus_item = event.srcElement;
      lastfocus_item = globalfocus_item;
      // event.srcElement.style.background = '#CEEDFB';
      event.srcElement.className = 'list_item list_item_focus';
      return true;
    }
    return true;
  }        
  
  function list_detail(event,n)
  {
    var k = document.getElementById('list_detail_'+n), p = document.getElementById('list_expand_'+n);
    if(k.style.display == 'block')
    {
      k.style.display = 'none';
      p.style.backgroundPosition = "-22px 0";
      // p.style.backgroundImage = 'url(""xml\\btn_expand.gif")';
    }
    else
    {
      k.style.display = 'block';
      p.style.backgroundPosition = "0 0";
      // p.style.backgroundImage = 'url("xml\\btn_contract.gif")';
    }
    document.getElementById('list_'+n).focus();
  }