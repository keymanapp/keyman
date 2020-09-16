var menudiv = null, global_menu_elem_name = null;
var globalfocus = null, globalfocus_item = null, lastfocus_item = null;

function doAttachEvent(obj, e, f)
{
  if(obj.attachEvent) return obj.attachEvent('on'+e, f);
  return obj.addEventListener(e, f, false);
}

function _$(e)
{
  return document.getElementById(e);
}

function windowResize()
{
  var
    eContent = _$('contentframe'),
    eFooter = _$('footerframe'),
    eMenu = _$('menuframe');

  if(eContent && eFooter && eMenu)
  {
    eContent.style.height = (document.body.offsetHeight - eFooter.offsetHeight - 1) + 'px';
    eContent.style.width = (document.body.offsetWidth - eMenu.offsetWidth) + 'px';
    eContent.style.left = eMenu.offsetWidth + 'px';
    eContent.style.top = 0 + 'px';
    eMenu.style.height = (document.body.offsetHeight - eFooter.offsetHeight - eMenu.offsetTop) + 'px';

    var h = (eContent.offsetHeight - 37 - 2) + 'px'; // header height and border height
    _$('subcontent_keyboards').style.height = h;
    _$('subcontent_options').style.height = h;
    var e = _$('subcontent_hotkeys');
    if(e) e.style.height = h;
    e = _$('subcontent_pro');
    if(e) e.style.height = h;
    _$('subcontent_support').style.height = h;
    _$('subcontent_keepintouch').style.height = h;
  }
}

doAttachEvent(window, 'resize', windowResize);
doAttachEvent(window, 'load', windowResize);
document.addEventListener("DOMContentLoaded", windowResize);

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
    return true;
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
    var itemtype, blah, elemid;

    globalfocus = event.srcElement;
    if(!event.srcElement) { return true; }
    elemid = event.srcElement.id;
    if(typeof elemid != 'string') {
      // e.g. if document has received focus, id may be undefined
      return true;
    }
    itemtype = elemid.substr(0, 5);

    if(event.srcElement.className.substr(0,8) != 'menuitem' && event.srcElement != menudiv
          && menudiv != null && menudiv.style.visibility=='visible') HideMenu();

    if(globalfocus_item != null) {
      if(globalfocus_item != event.srcElement && !globalfocus_item.contains(event.srcElement)) {
        /* globalfocus_item.style.background = '';
        globalfocus_item.style.filter = ''; */
        $(globalfocus_item).removeClass('list_item_focus');
        globalfocus_item = null;
      }
    }
    if( event.srcElement.className.indexOf('list_item') >= 0 ) {
      globalfocus_item = event.srcElement;
      lastfocus_item = globalfocus_item;
      // event.srcElement.style.background = '#CEEDFB';
      $(event.srcElement).addClass('list_item_focus');
      return true;
    }
    return true;
  }

  function list_detail(event,n) {
    var k = document.getElementById('list_'+n);
    $(k).toggleClass('expanded');
    k.focus();
    save_state();
    return false;
  }

  var loading_state = false;

  function save_state() {
    if(loading_state) return;
    var state = {
      activeIndex: menuframe_activeindex,
      keyboards: [],
      // TODO: scrollTop is not known when element is display:none
      keyboardsScrollTop: $('#subcontent_keyboards').scrollTop(),
      optionsScrollTop: $('#subcontent_options').scrollTop(),
      hotkeysScrollTop: $('#subcontent_hotkeys').scrollTop()
    };
    var keyboards = $('div.list_item');
    keyboards.each(function(index) {
      const name = $(this).data('name');
      if(name) {
        state.keyboards.push({name: name, expanded: $(this).hasClass('expanded')});
      }
    });
    const stateJson = JSON.stringify(state);
    const params = new URLSearchParams(window.location.search);
    const PageTag = params.get('tag');
    jQuery.post('/data/keyman/state', { tag: PageTag, state: stateJson });
  }

  function load_state() {
    loading_state = true;
    var s = $('#state').text();
    if(/^[0-9]+$/.exec(s)) {
      menuframe_activeindex = s;
      menuframe_activate(menuframe_activeindex);
    } else if(s != "") {
      var state = JSON.parse(s);
      if(!state) return;
      if(typeof state.activeIndex != 'undefined') {
        menuframe_activeindex = state.activeIndex;
        menuframe_activate(menuframe_activeindex);
      }
      if(state.keyboards) {
        for(var i = 0; i < state.keyboards.length; i++) {
          if(state.keyboards[i].expanded)
            $('div.list_item[data-name="'+state.keyboards[i].name+'"]').addClass('expanded');
        }
      }
      window.setTimeout(function() {
        if(state.keyboardsScrollTop) {
          $('#subcontent_keyboards').scrollTop(state.keyboardsScrollTop);
        }
        if(state.optionsScrollTop) {
          $('#subcontent_options').scrollTop(state.optionsScrollTop);
        }
        if(state.hotkeysScrollTop) {
          $('#subcontent_hotkeys').scrollTop(state.hotkeysScrollTop);
        }
      }, 1);
    }
    loading_state = false;
  }

  $( document ).ready(function() {
    load_state();
    $('#subcontent_keyboards').scroll(function() { save_state(); });
    $('#subcontent_options').scroll(function() { save_state(); });
    $('#subcontent_hotkeys').scroll(function() { save_state(); });
  });


function submitSupportRequest() {
  location.href = 'keyman:contact_support?message='+encodeURIComponent($('#contact_support').val());
}

/* Options tab */

function options_list_mouseover(n) {
  return true;
}
function options_list_mouseout(n) {
  return true;
}

function options_updatecheck(n) {
  var k = document.getElementById('optionscheck_'+n);
  k.checked = !k.checked;
  location.href='keyman:options_clickcheck?id='+n+'&value='+k.checked;
  document.getElementById('list_option_'+n).focus();
}

function options_basekeyboard() {
  location.href='keyman:options_basekeyboard';
}

/* Keyboards tab */

function keyboard_checkclick(n,toggle) {
  var k = document.getElementById('keyboardcheck_'+n), li = document.getElementById('list_keyboard_'+n);
  if(toggle) k.checked = !k.checked;
  location.href='keyman:keyboard_clickcheck?id='+n+'&value='+k.checked;
  var liTitle = document.getElementById('listtitle_keyboard_'+n);
  liTitle.className = k.checked ? 'list_title keyboard_loaded' : 'list_title keyboard_unloaded';
  li.focus();
  return true;
}

/* QRCode popup */

function showKeyboardLink(id) {
  var e = document.getElementById('qrcode-'+id);
  e.className='qrcode qrcode-visible';
}

function hideKeyboardLink(id) {
  var e = document.getElementById('qrcode-'+id);
  e.className='qrcode';
}