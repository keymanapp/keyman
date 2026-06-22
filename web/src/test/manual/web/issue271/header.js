function setActiveKeyboard() {
  var sKbd = document.getElementById('kbd_id4').value;
  var sLng = document.getElementById('lang_id4').value;
  var result = keyman.setActiveKeyboard("Keyboard_" + sKbd, sLng);
}