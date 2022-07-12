var tabselected = -1;
var showHelp = true;
var state = null;

var MAX_TABS = 5;

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
    case 0: title = 'Information'; break;
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

function showhideupper()
{
  var n = document.getElementById('upperexpand').className;
  document.getElementById('upperexpand').className = (n=='checkbox'?'checkbox checked':'checkbox');
  for ( var i=0; i < MAX_TABS; i++ ) {
    document.getElementById('uppertext'+i).style.display = (n=='checkbox'?'block':'none');
  }
  savestate();
}

function isTabVisible(n) {
  return document.getElementById('tabb'+n).offsetParent != null;
}

document.onkeydown = function()
{
  switch(event.keyCode)
  {
  case 33: //pgup
    if(tabselected > 0) {
      do {
        selecttabb(tabselected-1);
      } while(!isTabVisible(tabselected) && tabselected > 0);
    }
    break;
  case 34: //pgdn
    if(tabselected < MAX_TABS - 1) {
      do {
        selecttabb(tabselected+1);
      } while(!isTabVisible(tabselected) && tabselected < MAX_TABS - 1)
    }
    break;
  default:
    event.returnValue = true;
    return true;
  }
  event.cancelBubble = true;
  event.returnValue = false;
  return false;
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
  state = document.getElementById('state');
  loadingState = true;

  var nodes = state.getElementsByTagName('tab');
  if(nodes.length > 0) selecttabb(Number(nodes[0].textContent));

  // Show/hide help
  nodes = state.getElementsByTagName('showhelp');
  if((nodes.length > 0) && (nodes[0].textContent == '0')) {
    document.getElementById('upperexpand').className = 'checkbox';
  } else {
    document.getElementById('upperexpand').className = 'checkbox checked';
  }

  var n = document.getElementById('upperexpand').className;
  for (var i=0; i < MAX_TABS; i++ ) {
    document.getElementById('uppertext'+i).style.display = (n=='checkbox'?'none':'block');
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
