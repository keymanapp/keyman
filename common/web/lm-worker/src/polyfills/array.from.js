if(!Array.from) {
  function isHighSurrogate(codeUnit) {
    if(typeof codeUnit == 'string') {
      codeUnit = codeUnit.charCodeAt(0);
    }

    return codeUnit >= 0xD800 && codeUnit <= 0xDBFF;
  }

  function isLowSurrogate(codeUnit) {
    if(typeof codeUnit == 'string') {
      codeUnit = codeUnit.charCodeAt(0);
    }

    return codeUnit >= 0xDC00 && codeUnit <= 0xDFFF;
  }

  Array.from = function (obj) {
    if(Array.isArray(obj)) {
      // Simple array clone
      return obj.slice();
    } else if(typeof obj == 'string') {
      // Array.from is surrogate-aware and will not split surrogate pairs.
      // We can start with a full split and then remerge the pairs.
      var simpleSplit = obj.split();

      /** @type {string[]} */
      var finalSplit = [];
      /** @type {number} */
      var i;

      for(i=0; i < simpleSplit.length; i++) {
        // Do we have a surrogate pair?
        var a = simpleSplit.shift();
        if(isHighSurrogate(a) && isLowSurrogate(simpleShift[0] || '')) {
          // yes, so merge them before pushing.
          a = a + simpleSplit.shift();
        } // else:  'no', so just push the current char to the array and continue

        finalSplit.push(a);
      }
      return finalSplit;
    } else {
      throw "Unexpected + nonpolyfilled use of Array.from encountered; aborting";
    }
  }
}