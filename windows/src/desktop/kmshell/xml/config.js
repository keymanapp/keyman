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

    var h = (eContent.offsetHeight - 40 - 2) + 'px'; // header height and border height
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
    case 27:    // esc

      const id = n.substring(9); // remove keyboard_ from element name
      const qr_id = document.getElementById('qrcode-'+id);
      const modify_id = document.getElementById('modify-'+id);
      if ((modify_id) && $(modify_id).hasClass('modify_visible')){
        hideModifyLink(id);
      }
      else if ((qr_id) && $(qr_id).hasClass('qrcode_visible')){
        hideKeyboardLink(id);
      }
      break;
    case 35:    // end
      var elem = event.srcElement.parentElement;
      for(var i = elem.children.length - 1; i >= 0; i--) {
        if(elem.children[i].tabIndex != undefined && elem.children[i].tabIndex >= 0) {
          elem.children[i].focus();
          break;
        }
      }
      break;
    case 36:    // home
      var elem = event.srcElement.parentElement;
      for( var i = 0; i < elem.children.length; i++ ) {
        if( elem.children[i].tabIndex != undefined && elem.children[i].tabIndex >= 0 ) {
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
            if(elem.children[j].tabIndex != undefined && elem.children[j].tabIndex >= 0)
            {
              elem.children[j].focus();
              list_expanded_selected(elem.children[j]);
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
            if(elem.children[j].tabIndex != undefined && elem.children[j].tabIndex >= 0)
            {
              elem.children[j].focus();
              list_expanded_selected(elem.children[j]);
              break;
            }
          } while(j != i);
          break;
        }
      }
      break;
    case 37:    // left
    case 109:   // minus
      {
        const kbd_list = document.getElementById('list_'+n);
        if( !kbd_list ) {
          break;
        }
        if(!kbd_list.classList.contains('expanded')){
          break;
        }
        //  hide more details for the expanded list
        const keyboard_name = n.substring(9); // remove keyboard_ from element name
        const kbd_grid = document.getElementById('keyboard_grid_'+keyboard_name);
        if( !kbd_grid ) break;
        if (!$(kbd_grid).hasClass('grid_rows_hide')){
          more_detail_toggle(kbd_grid.id);
        }
      }
      break;
    case 39:    // right
    case 107:   // plus
      {
        const kbd_list = document.getElementById('list_'+n);
        if( !kbd_list ) {
          break;
        }
        if(!kbd_list.classList.contains('expanded')){
          break;
        }
        //  show more details for the expanded list
        const keyboard_name = n.substring(9); // remove keyboard_ from element name
        const kbd_grid = document.getElementById('keyboard_grid_'+keyboard_name);
        if( !kbd_grid ) break;
        if ($(kbd_grid).hasClass('grid_rows_hide')){
          more_detail_toggle(kbd_grid.id);
        }
      }
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

  function list_detail(event,kbd_name) {
    var kbd_list_item = document.getElementById('list_'+kbd_name);
    list_expanded_selected (kbd_list_item);
    kbd_list_item.focus();
    save_state();
    return false;
  }

  function list_expanded_selected (kbd_list_item){
    if(loading_state) return;
    const keyboards = $('div.list_item');
    keyboards.each(function(index) {
      if(this.id == kbd_list_item.id) {
        $(this).toggleClass('expanded');
      }
      else {
        $(this).removeClass('expanded');
      }
    });
  }

  function more_detail_toggle(kbd_grid_name) {
    const kbd_grid_item = document.getElementById(kbd_grid_name);
    const more_detail_input = document.getElementById(kbd_grid_name+'_more');
    $(kbd_grid_item).toggleClass('grid_rows_hide');
    if ($(kbd_grid_item).hasClass('grid_rows_hide')){
      $(more_detail_input).attr("src","/app/expand20.png");
    }
    else {
      $(more_detail_input).attr("src","/app/collapse20.png");
    }
  }

  var loading_state = false;

  function save_state() {
    if(loading_state) return;
    var state = {
      activeIndex: menuframe_activeindex,
      keyboards: [],
      modify_languages: [],
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
    var modify_list = $('div.modify');
    modify_list.each(function(index) {
      const name = $(this).data('name');
      if(name) {
        state.modify_languages.push({name: name, modify: $(this).hasClass('modify_visible')});
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
      if(state.modify_languages) {
        for(var i = 0; i < state.modify_languages.length; i++) {
          if(state.modify_languages[i].modify)
            $('div.modify[data-name="'+state.modify_languages[i].name+'"]').addClass('modify_visible');
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

/* Disable/Enable Button replaces checkbox for now still use the "check" function in delphi webserver */
function keyboard_toggle(keyboard_name) {
  const checkbox = document.getElementById('keyboardcheck_'+keyboard_name), li = document.getElementById('list_keyboard_'+keyboard_name);
  const grid_container = document.getElementById('keyboard_grid_'+keyboard_name);
  const disable_btn = document.getElementById('button_disable_'+keyboard_name);
  const enable_btn = document.getElementById('button_enable_'+keyboard_name);
  const add_remove_btn = document.getElementById('button_add_remove_'+keyboard_name);

  checkbox.checked = !checkbox.checked;
  if (checkbox.checked){
    enable_btn.style.display = "none";
    disable_btn.style.display = "block";
    grid_container.classList.remove("grid_disabled");
    add_remove_btn.disabled = false;
  }
  else {
    disable_btn.style.display = "none";
    enable_btn.style.display = "block";
    grid_container.classList.add("grid_disabled");
    add_remove_btn.disabled = true;
  }

  location.href='keyman:keyboard_clickcheck?id='+keyboard_name+'&value='+checkbox.checked;
  var liTitle = document.getElementById('listtitle_keyboard_'+keyboard_name);
  liTitle.className = checkbox.checked ? 'list_title keyboard_loaded flex_container_title' : 'list_title keyboard_unloaded flex_container_title';
  li.focus();
  return true;
}

/* QRCode popup */

function showKeyboardLink(id) {
  var e = document.getElementById('qrcode-'+id);
  if(window.Sentry) {
    // trace for KEYMAN-WINDOWS-4R
    window.Sentry.addBreadcrumb({category:'trace', message:`showKeyboardLink:id=${id}, e=${e?'defined':'undefined'}`, level: 'info'});
  }
  e.className='qrcode qrcode_visible';
}

function hideKeyboardLink(id) {
  var e = document.getElementById('qrcode-'+id);
  e.className='qrcode';
}

/* Modify Languages Popup */
function showModifyLink(id) {
  var e = document.getElementById('modify-'+id);
  e.className='modify modify_visible';
  save_state();
}
function hideModifyLink(id) {
  var e = document.getElementById('modify-'+id);
  e.className='modify';
  save_state();
}
