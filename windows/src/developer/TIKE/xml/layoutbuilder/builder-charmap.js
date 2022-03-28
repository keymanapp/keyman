//
// Character map drag+drop and double-click insertion
//

builder.dragDrop = {};
(function(builder, dragDrop) {
  dragDrop.isSubKey = function(key) {
    return key.parent().attr('id') == 'sk';
  };

  dragDrop.getKeyElementFromElement = function(elem) {
    if(elem.classList.contains('key')) {
      return elem;
    }

    if(!elem.parentElement) {
      return null;
    }

    if(elem.parentElement.classList.contains('key')) {
      return elem.parentElement;
    }

    return null;
  };

  dragDrop.getKeyElementFromDragTarget = function(target) {
    let key = dragDrop.getKeyElementFromElement(target);
    let isIdField = false;

    if(key) {
      key = $(key);
    } else {
      if(target.id == 'inpKeyCap') {
        key = builder.selectedKey();
      } else if(target.id == 'inpKeyName') {
        key = builder.selectedKey();
        isIdField = true;
      } else if(target.id == 'inpSubKeyCap') {
        key = builder.selectedSubKey();
      } else if(target.id == 'inpSubKeyName') {
        key = builder.selectedSubKey();
        isIdField = true;
      }

      if(!key) {
        return null;
      }
    }

    return { key: key, isIdField: isIdField };
  };

  dragDrop.isHighSurrogate = function(character) {
    let codeUnit = character.charCodeAt(0);
    return codeUnit >= 0xD800 && codeUnit <= 0xDBFF;
  };

  dragDrop.isLowSurrogate = function(character) {
    let codeUnit = character.charCodeAt(0);
    return codeUnit >= 0xDC00 && codeUnit <= 0xDFFF;
  };

  dragDrop.surrogatePairToValue = function(character) {
    let h = character.charCodeAt(0), l = character.charCodeAt(1);
    return 0x10000 + (h - 0xD800) * 0x400 + (l - 0xDC00);
  };

  dragDrop.charToUnicodeValue = function(s) {
    let v;
    if(s.length == 2 && dragDrop.isHighSurrogate(s) && dragDrop.isLowSurrogate(s.substring(1))) {
      v = builder.surrogatePairToValue(s);
    } else if(s.length == 1) {
      v = s.charCodeAt(0);
    } else {
      return null;
    }

    let ch = v.toString(16);
    while(ch.length < 4) ch = '0' + ch;
    return ch;
  };

  dragDrop.keySelectTimeout = 0;

  dragDrop.clearKeySelectTimeout = function() {
    if(dragDrop.keySelectTimeout) {
      window.clearTimeout(dragDrop.keySelectTimeout);
      dragDrop.keySelectTimeout = 0;
    }
  };

  builder.charmapDragOver = function(o) {
    // Convert X, Y to document coordinates
    dragDrop.clearKeySelectTimeout();

    const target = document.elementFromPoint(o.x, o.y);
    const key = target ? dragDrop.getKeyElementFromDragTarget(target) : null;
    const found = key && key.key && key.key.length;

    dragDrop.keySelectTimeout = window.setTimeout(function() {
      // We use a timeout to allow dragging over to a key, then
      // pausing to select it, and then dragging to a subkey of
      // that key.
      if(!found) {
        builder.selectKey(null);
      } else if(dragDrop.isSubKey(key.key)) {
        builder.selectSubKey(key.key);
      } else {
        builder.selectKey(key.key);
      }
      dragDrop.keySelectTimeout = 0;
    },
    // 1.0 sec if moving onto a blank space feels a little more
    // friendly. We can pause a bit longer near the subkey array
    // and not have it disappear
    found ? 250 : 1000);

    return found;
  };

  builder.charmapDragDrop = function(o) {

    let key = null;

    dragDrop.clearKeySelectTimeout();

    if(o.ctrl) {
      // Ctrl as part of a drag/drop interaction
      // so cancel the default select-key dialog
      builder.ctrlDown = false;
      // console.log('charmapDragDrop ctrlDown=false');
    }

    if(o.shift && o.ctrl) {
      // o.shift means append to current key cap
      // o.ctrl means set id value
      // together they have no good meaning
      return false;
    }

    // Convert X, Y to document coordinates

    if(o.x >= 0 && o.y >= 0) {
      let target = document.elementFromPoint(o.x, o.y);
      if(target != null) {
        key = dragDrop.getKeyElementFromDragTarget(target);

        if(key && key.key && key.key.length) {
          if(dragDrop.isSubKey(key.key)) {
            builder.selectSubKey(key.key);
          } else {
            builder.selectKey(key.key);
          }
        }
      }
    } else {
      // Double-click insertion, so use last focused control
      key = { key: builder.selectedSubKey(), isIdField: false };
      if(!key.key || !key.key.length) {
        key = { key: builder.selectedKey(), isIdField: false };
      }
    }

    if(!key || !key.key || !key.key.length) {
      return false;
    }

    // Focus the control and add the text
    if(key.isIdField || key.key.data('id').startsWith('T_new_') || o.ctrl) {
      let target = $(dragDrop.isSubKey(key.key) ? '#inpSubKeyName' : '#inpKeyName');
      target.val('U_'+dragDrop.charToUnicodeValue(o.text).toUpperCase());

      if(key.isIdField) {
        target.focus();
      }

      target.change();
    }

    if(!key.isIdField) {
      let target = $(dragDrop.isSubKey(key.key) ? '#inpSubKeyCap' : '#inpKeyCap');
      target.focus();
      target.val((o.shift ? target.val() : '') + o.text);
      target.change();
    }
  };
})(builder, builder.dragDrop);
