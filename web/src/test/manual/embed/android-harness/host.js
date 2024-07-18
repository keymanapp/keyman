let x = $('#iframe')[0];
let ta = $('#ta')[0];

const TEST_KEYBOARD = {
  KN: 'web_context_tests',
  KI: 'Keyboard_web_context_tests',
  KLC: 'en',
  KL: 'English',
  KF: 'web_context_tests.js',
  KP: 'web_context_tests'
};

window.jsInterface = {
  getDeviceType: function() {
    return 'AndroidMobile'; // 'AndroidTablet'
  },
  getDefaultBannerHeight: function() {
    return '50px'; // 40dp, landscape, 60dp, 90dp f
  },
  getKeyboardHeight: function() {
    return '280dp'; //150-405dp
  },
  getKeyboardWidth: function() {
    return '240dp';
  },
  beepKeyboard: function() {
    //
  },
  setIsChiral(isChiral) {
    //
  },
  /**
   * Inserts the selected string <i>s</i>
   * @param dn  Number of pre-caret code points (UTF+8 characters) to delete
   * @param s   Text to insert
   * @param dr  Number of post-caret code points to delete.  (optional)
   */
  insertText(dn, s, dr) {
    console.log('insertText', dn, s, dr); //document.getElementById()
    let v = ta.value;

    let sel = getSelection();

    let left = v.substring(0, sel.start - dn);
    let right = v.substring(sel.start + sel.length);
    // TODO: dr
    ta.value = left + s + right;
    let selStart = sel.start + s.length;
    $('#selStart').val(selStart.toString());
    $('#selLength').val('0');
  },
  dispatchKey(code, eventModifiers) {
    console.log('dispatchKey', code, eventModifiers);
  },
  initialKeyboard() {
    return JSON.stringify(TEST_KEYBOARD);
  }
};

x.onload = function() {
  x.contentWindow.setOskWidth("240");
  x.contentWindow.setOskHeight("280");
}
x.src='host/keyboard.html';

function getSelection() {
  let selStart = parseInt($('#selStart').val(),10), selLength = parseInt($('#selLength').val(),10);
  return {start: selStart, length: selLength};
}

function updateTextAndSelection() {
  let selStart = parseInt($('#selStart').val(),10), selLength = parseInt($('#selLength').val(),10);
  let sel = $('#selection');
  let selText = ta.value.substring(selStart, selStart+selLength);
  sel.text(selText);
  console.log('Setting content to "'+ta.value+'"');
  x.contentWindow.updateKMText(ta.value);
  x.contentWindow.updateKMSelectionRange(selStart, selStart+selLength);
}

$('#selChange').on('click', updateTextAndSelection);
$('#ta').on('keyup', updateTextAndSelection);
