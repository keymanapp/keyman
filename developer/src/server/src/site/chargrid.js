"use strict";

/* Character Grid */

(function() {
  const charGrid = document.getElementById('character-grid');

  var lastContent = null;

  function removeChildNodes(node) {
    while (node.lastChild) {
      node.removeChild(node.lastChild);
    }
  }

  function addCharElements(text, code) {
    var ebox = document.createElement('div'), echar = document.createElement('div'), ecode = document.createElement('div');
    echar.textContent = text;
    echar.className = 'char-char keymanweb-font';
    ecode.textContent = code;
    ecode.className = 'char-code';
    ebox.appendChild(echar);
    ebox.appendChild(ecode);
    charGrid.appendChild(ebox);
  }

  function logContent() {
    if(lastContent === ta1.value) {
      updateLogCursor();
      return;
    }
    removeChildNodes(charGrid);
    if(ta1.value.length == 0) {
      addCharElements('-','empty');
    } else {
      for(var i = 0; i < ta1.value.length; i++) {
        //
        var code = ta1.value.charCodeAt(i);
        var text = ta1.value.charAt(i);
        var slice = 4;
        // Test for SMP
        if(code >= 0xD800 && code < 0xDC00) {
          if(i < ta1.value.length) {
            var code2 = ta1.value.charCodeAt(i+1);
            if(code2 >= 0xDC00 && code < 0xE000) {
              code = (code - 0xD800) * 0x400 + (code2 - 0xDC00) + 0x10000;
              text += ta1.value.charAt(i+1);
              slice = 6;
              i++;
            }
          }
        }
        addCharElements(text, ('000000'+(code).toString(16)).slice(-slice));
      }
    }
    updateLogCursor();
    lastContent = ta1.value;
  }

  var lastSelStart = -1, lastSelLength = -1;

  function calculateLengthByCodepoint(text, base, x)  {
    var stop = base + x;
    while(base < stop - 1) {
      if(text.charCodeAt(base) >= 0xD800 && text.charCodeAt(base) < 0xDC00 &&
        text.charCodeAt(base+1) >= 0xDC00 && text.charCodeAt(base+1) < 0xE000) {
        // Decrement position by one for each surrogate pair
        x--;
      }
      base++;
    }
    return x;
  }

  function updateLogCursor() {
    var i, selStart, selLength, selDirection;

    // We use the position reported by the textarea control
    selStart = ta1.selectionStart;
    selLength = ta1.selectionEnd - ta1.selectionStart;
    selDirection = ta1.selectionDirection;

    selLength = calculateLengthByCodepoint(ta1.value, selStart, selLength);
    selStart = calculateLengthByCodepoint(ta1.value, 0, selStart);

    //console.log('selStart='+selStart+', selLength='+selLength);
    if(lastSelStart != selStart || lastSelLength != selLength) {
      for(i = 0; i < charGrid.childNodes.length; i++) {
        charGrid.childNodes[i].className = '';
      }

      var x = selDirection == 'backward' ? selStart-1 : selStart+selLength - 1;

      if(x < 0) {
        charGrid.className = 'cursor';
      } else {
        charGrid.className = '';
        if(x >= 0 && x < charGrid.childNodes.length) {
          charGrid.childNodes[x].className = 'cursor';
        }
        if(keyman.util.isTouchDevice() || charGrid.scrollHeight > charGrid.clientHeight) {
          charGrid.childNodes[x].scrollIntoView();
          // This scrollIntoView call can cause the document to scroll as well, even though
          // we've said overflow:hidden. So this restores the scroll position in the document.
          document.body.scrollTop = 0;
        }
      }

      for(i = selStart; i < selStart+selLength; i++) {
        charGrid.childNodes[i].className += ' cursor-selected';
      }
      lastSelStart = selStart;
      lastSelLength = selLength;
    }
  }

  logContent();
  window.setInterval(logContent, 100);
})();