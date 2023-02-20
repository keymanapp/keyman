function setActiveKeyboard() {
  var kmw=window['keyman'] ? keyman : tavultesoft.keymanweb;

  var sKbd = document.getElementById('kbd_id4').value;
  var sLng = document.getElementById('lang_id4').value;
  var result = kmw.setActiveKeyboard("Keyboard_" + sKbd, sLng);
}