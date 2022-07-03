//
// Character map drag+drop and double-click insertion
//

builder.dragDrop = {};
(function(builder, dragDrop) {
  dragDrop.isSubKey = function(key) {
    return key.data('type') == 'longpress';
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

  dragDrop.charToUnicodeValue = function(s) {
    let v = s.codePointAt(0);
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
      if($.contains($('#subKeyToolbar')[0], builder.lastFocus)) {
        key = { key: builder.selectedSubKey(), isIdField: false };
      } else {
        key = { key: builder.selectedKey(), isIdField: false };
      }
    }

    if(!key || !key.key || !key.key.length) {
      return false;
    }

    const target = $(dragDrop.isSubKey(key.key) ? '#inpSubKeyCap' : '#inpKeyCap');
    const text = (o.shift ? target.val() : '') + o.text;

    // Focus the control and add the text
    if(key.isIdField || key.key.data('id').startsWith('T_new_') || o.ctrl) {
      const idTarget = $(dragDrop.isSubKey(key.key) ? '#inpSubKeyName' : '#inpKeyName');
      const chars = [...text]; // Split o.text into array of codepoints, keeping surrogate pairs together
      let id = chars.reduce((prev,curr) => prev+'_'+dragDrop.charToUnicodeValue(curr).toUpperCase(), 'U');
      idTarget.val(id);

      if(key.isIdField) {
        idTarget.focus();
      }
      idTarget.change();
    }

    if(!key.isIdField) {
      target.focus();
      target.val(text);
      target.change();
    }
  };
})(builder, builder.dragDrop);
