var globalfocus=null, globalfocus_file=null;
var mouseisdown = null;
var tabselected = -1;
var state = null;
var showHelp = true;

var projectPath = (function(s) {
  var search = s.match(/path=(.+?)(&|$)/);
  return search ? decodeURIComponent(search[1]) : null;
})(window.location.search);

function updateBodyClass() {
  document.body.className = 'tab-'+tabselected+(showHelp ? ' show-help' : '');
}

function selecttabb(n) {
  tabselected = n;
  updateBodyClass();
    
  var title='';
  switch (n) {
    case 0: title = 'Welcome'; break;
    case 1: title = 'Keyboards'; break;
    case 2: title = 'Models'; break;
    case 3: title = 'Packaging'; break;
    case 4: title = 'Distribution'; break;
    default: alert(n); return;
  }

  document.getElementById('currentpage').innerHTML = 'Project - '+title;
  q = document.getElementById('page-'+n).getElementsByTagName('span');
  for(var i = 0; i < q.length; i++)
    if(q[i].className=='file')
    {
      q[i].focus();
      break;
    }
  savestate();
}

function pageload() {
  for(var i = 0; i<document.all.length; i++)
    document.all[i].unselectable = 'on';
  var kbuttons = document.getElementsByTagName('button');
  for(var b=0; b<kbuttons.length; b++) {
    kbuttons[b].onmouseover = function()
    { 
      if(this.className == 'disabled') return false;
      if(mouseisdown == null || mouseisdown == this)
        this.className=(mouseisdown==this?'down':'hover'); return true;
    }
    kbuttons[b].onmouseout = function()
    { 
      if(this.className == 'disabled') return false;
      this.className=''; return true;
    }
    kbuttons[b].onmousedown = function()
    {
      if(event.button != 1) return false;
      if(this.className == 'disabled') return false;
      this.focus();
      this.className='down'; 
      mouseisdown=this; 
      return true;
    }
    
    kbuttons[b].onmouseup = function()
    {
      if(this.className == 'disabled') return false;
      this.className='hover';
      if(mouseisdown == this)
      {
        location.href=this.command;
      }
      mouseisdown=null;
      return true; 
    }
    kbuttons[b].onkeypress = function() { this.className=(window.event.keyCode==32?'down':''); return true; }
    kbuttons[b].onkeyup = function() { this.className='hover'; if(window.event.keyCode==32) location.href=this.command; return true; }
  }
  state = document.getElementById('state');
  loadstate();
}

document.addEventListener('mouseup', function() { mouseisdown = null; return true; }, false);
function showfiledetails(x) {
  var n = document.getElementById('fileplus'+x).className;
  document.getElementById('fileplus'+x).className = (n=='fileexpand'?'fileexpand filecontract':'fileexpand');
  document.getElementById('filedetails'+x).style.display = (n=='fileexpand'?'block':'none');
  savestate();
}
//function expandfiledetails(x) {
//}
function hoverfile(x) {
  document.getElementById('file'+x).style.background = (globalfocus==document.getElementById('file'+x)?'#CEEDFB':'ECF8FE');
}
function unhoverfile(x) {
  document.getElementById('file'+x).style.background = (globalfocus==document.getElementById('file'+x)?'#CEEDFB':'');
}
function showhideupper()
{
  var n = document.getElementById('upperexpand').className;
  document.getElementById('upperexpand').className = (n=='checkbox'?'checkbox checked':'checkbox');
  for ( var i=0; i<4; i++ ) {
    document.getElementById('uppertext'+i).style.display = (n=='checkbox'?'block':'none');
  }
  savestate();
}
document.onkeydown = function()
{
  switch(event.keyCode)
  {
  case 33: //pgup
    if(tabselected > 0) selecttabb(tabselected-1);
    break;
  case 34: //pgdn
    if(tabselected < 3) selecttabb(tabselected+1);
    break;
  default:
    event.returnValue = true;
    return true;
  }
  event.cancelBubble = true;
  event.returnValue = false;
  return false;
}

function getFileElementFromEvent()
{
  var elem = event.srcElement;
  while(elem && elem.id.substr(0,7) != 'fileid_') elem = elem.parentElement;
  return elem;
}

function file_mousedown()
{
  var x = getFileElementFromEvent();
  if(x == null) return true;
  x = x.id.substr(4);
  /* Use the same menu from the options button */
  menu = document.getElementById('menu_options_'+x);
  if(menu != null)
  {
    event.srcElement.focus();
    menudiv.innerHTML = menu.innerHTML;
    with(PageOffset(event.srcElement)) {
      menudiv.style.left = x + event.offsetX;
      menudiv.style.top = y + event.offsetY;

      menudiv.style.width='150px';
      menudiv.style.zIndex=100;
      menudiv.style.visibility='visible';
      document.getElementById('menubackground').className = 'show';
    }
  }
  event.cancelBubble = true;
  event.returnValue = false;
  return false;
}

function file_keydown()
{
  var x = event.srcElement.id.substr(4);
  switch(event.keyCode)
  {
  case 38:
    // find prev file element
    var elem = event.srcElement.parentElement;
    for(var i = 0; i < elem.children.length; i++)
      if(elem.children[i] == event.srcElement)
      {
        if(i == 0) elem.children[elem.children.length-1].focus();
        else elem.children[i-1].focus();
        break;
      }
    break;
  case 40:
    // find next file element
    var elem = event.srcElement.parentElement;
    for(var i = 0; i < elem.children.length; i++)
      if(elem.children[i] == event.srcElement)
      {
        if(i == elem.children.length-1) elem.children[0].focus();
        else elem.children[i+1].focus();
        break;
      }
    break;
  case 37: // left
  case 109: // -
    if(document.getElementById('fileplus'+x))
    {
      document.getElementById('fileplus'+x).className = 'fileexpand';
      document.getElementById('filedetails'+x).style.display = 'none';
    }
    break;
  case 13:
    location.href='keyman:editfile?id='+x;
  case 39: // right
  case 107: // +
    if(document.getElementById('fileplus'+x))
    {
      document.getElementById('fileplus'+x).className = 'fileexpand filecontract';
      document.getElementById('filedetails'+x).style.display = 'block';
    }
    break;
  default:
//        alert(event.keyCode);
    event.returnValue = true;
    return true;
  }
  savestate();
  event.returnValue = false;
  event.cancelBubble = true;
  return false;
}
document.onfocusin = function()
{
  globalfocus = event.srcElement;
  if(globalfocus_file != null)
  {
    if(globalfocus_file != event.srcElement && !globalfocus_file.contains(event.srcElement))
    {
      globalfocus_file.style.background = '';
      globalfocus_file.style.filter = '';
      globalfocus_file = null;
    }        
  }
  if(event.srcElement.className == 'file')
  {
    globalfocus_file = event.srcElement;
    event.srcElement.style.background = '#CEEDFB';
    event.srcElement.style.filter = 'progid:DXImageTransform.Microsoft.Gradient(gradientType=1,startColorStr=#ceedfb,endColorStr=#F0F8Ff);';
    return true;
  }
  else if(event.srcElement.tagName == 'button')
  {
    globalfocus_file = null;
    event.srcElement.className = 'hover';
    return true;
  }
}
document.onfocusout = function()
{
  /*if(event.srcElement.className == 'file')
  {
    // if it is a file and the next focus is a child of this element, then don't blur it yet 
    event.srcElement.style.background = '';
    return true;
  }
  else*/ if(event.srcElement.tagName == 'button')
  {
    event.srcElement.className = '';
    return true;
  }
}

var loadingState = false;

function savestate() {
  if(loadingState) {
    return;
  }
  
  var appendElement = function(doc, tag, value) {
    var element = doc.createElement(tag);
    if(value) {
      element.appendChild(doc.createTextNode(value));
    }
    doc.documentElement.appendChild(element);
    return element;
  };
  
  var xmlDoc = document.implementation.createDocument(null, "state");

  appendElement(xmlDoc, 'tab', tabselected.toString());
  appendElement(xmlDoc, 'showhelp', document.getElementById('upperexpand').className == 'checkbox' ? "0":"1");
  appendElement(xmlDoc, 'path', projectPath);
  
  var files = document.getElementsByTagName('span');
  for(var i = 0; i < files.length; i++) {
    if(files[i].className == 'file') {
      var id = files[i].id.substr(4);
      var elem = document.getElementById('fileplus'+id);
      if(elem && elem.className != 'fileexpand') {
        var file = appendElement(xmlDoc, 'file');
        file.setAttribute('ID', id);
        file.setAttribute('Expanded', '1');
      }
    }
  }
  
  var serializer = new XMLSerializer();
  var xmlString = serializer.serializeToString(xmlDoc);

  var q = new XMLHttpRequest();
  q.open('POST', '/app/project/state');
  q.setRequestHeader('Content-Type', 'application/json; charset=UTF-8');
  q.send(xmlString);
}

function loadstate() {
  // Select active tab
  loadingState = true;
  
  var nodes = state.getElementsByTagName('tab');
  if(nodes.length > 0) selecttabb(Number(nodes[0].textContent));
  
  // Show/hide help
  nodes = state.getElementsByTagName('showhelp');
  if((nodes.length > 0) && (nodes[0].textContent == '0')) {
    document.getElementById('upperexpand').className = 'checkbox';
  } else {
    document.getElementById('upperexpand').className = 'checkbox checked';
    var n = document.getElementById('upperexpand').className;
    for (var i=0; i<4; i++ ) {
      document.getElementById('uppertext'+i).style.display = (n=='checkbox'?'none':'block');
    }
  }  

  // Expand files
  nodes = state.getElementsByTagName('file');
  for(var i = 0; i < nodes.length; i++) {
    var nodeID = nodes[i].attributes['ID'];
    var nodeExpansion = nodes[i].attributes['Expanded'];
    if(nodeID && nodeExpansion) {
      var elem = document.getElementById('fileplus'+nodeID.value);
      if(elem) elem.className = 'fileexpand filecontract';
      elem = document.getElementById('filedetails'+nodeID.value);
      if(elem) elem.style.display = 'block';
    }
  }
      
  if(tabselected == -1) {
    selecttabb(0);
  }
  
  loadingState = false;
}

var menudiv=null;

function PageOffset(e) {
  var r = e.getBoundingClientRect();
  return {x: r.left, y: r.top};
}

function ShowMenu(name,align) {
  var menu = document.getElementById('menu_'+name), button = document.getElementById('button_'+name);
  menudiv.innerHTML = menu.innerHTML;
  var pb = PageOffset(button);
  var pm = PageOffset(menu.offsetParent);

  menudiv.style.width='150px';
  menudiv.style.zIndex=100;
  menudiv.style.visibility='visible';
  if(align=='right') menudiv.style.left = (pb.x+button.offsetWidth-menudiv.offsetWidth) + 'px';
  else menudiv.style.left = pb.x + 'px'; 
  menudiv.style.top = (pb.y+button.offsetHeight) + 'px';
  
  document.getElementById('menubackground').className = 'show';
}

function HideMenu() {
  if(event.srcElement != menudiv) {
    menudiv.style.visibility='hidden';
    document.getElementById('menubackground').className = '';
  }
}

function menuitemdown() {
  var e = event.srcElement; while(e && e.tagName.toLowerCase() != 'k:menuitem') e=e.parentElement;
  if(e) {
    location.href=e.attributes['command'].value;
  }
}

function menusetup() {
  menudiv = document.createElement('DIV');
  menudiv.className='menu';
  menudiv.addEventListener('mousedown', HideMenu, false);
  document.body.appendChild(menudiv);
}

window.addEventListener('load', menusetup, false);
