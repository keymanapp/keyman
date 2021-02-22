function tabClick(tab) {
  var elemDetails = document.getElementById('details');
  var elemReadme = document.getElementById('readme');
  var tabDetails = document.getElementById('tabDetails');
  var tabReadme = document.getElementById('tabReadme');
  if(tab == tabDetails) {
    tabDetails.className = "selected";
    tabReadme.className = "";
    elemDetails.className = "content_selected";
    elemReadme.className = "content";
  } else {
    tabDetails.className = "";
    tabReadme.className = "selected";
    elemDetails.className = "content";
    elemReadme.className = "content_selected";
  }
}

function genkeydown(event) {
  if(!event) return true;
  if(event.keyCode == 13 && event.srcElement.tagName != 'A' && (!event.srcElement.type || event.srcElement.type != 'button')) {
    location.href='keyman:keyboard_install';
  } else if(event.keyCode == 27) {
    location.href='keyman:keyboard_cancel';
  } else {
    return true;
  }
  event.cancelBubble = true;
  event.returnValue = false;
  return false;
}

function framekeydown() {
  genkeydown(document.getElementById('frameReadme').contentWindow.event);
}

document.onkeydown = function() {
  return genkeydown(event);
}

function setupframehotkeys() {
  try {
    var e = document.getElementById('frameReadme').contentWindow.document;
    e.onkeydown = framekeydown;
  } catch(ex) { /* I1672 - ignore access denied when loading other links */ }
}

window.onload = function() {
  setupframehotkeys();

  if(document.getElementById('tabs') != null) {
    tabClick(document.getElementById('tabDetails'));
  }
}

function keyboard_install(elevate) {
  let href = elevate ? 'keyman:keyboard_installallusers?' : 'keyman:keyboard_install?';
  let keyboards = Array.from(document.querySelectorAll('select.keyboardLanguage'));
  location.href = href + keyboards.map((e) => e.id.substring('keyboardLanguage_'.length) + '=' + e.value).join('&');
}
