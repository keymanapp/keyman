import { assert } from 'chai';

import { Keyboard, KeyboardProcessor } from '@keymanapp/keyboard-processor';
import { extendString } from '@keymanapp/web-utils';

extendString();

let device = {
  formFactor: 'desktop',
  OS: 'windows',
  browser: 'native'
}

describe('Engine - Stores', function() {
  var toSupplementaryPairString = function(code){
    var H = Math.floor((code - 0x10000) / 0x400) + 0xD800;
    var L = (code - 0x10000) % 0x400 + 0xDC00;

    return String.fromCharCode(H, L);
  }

  it('Store \'Explosion\'', function() {
    let processor = new KeyboardProcessor(device);
    // A 'hollow' Keyboard that only follows default rules.  That said, we need a Keyboard
    // instance to host cache data for our exploded store tests.
    processor.activeKeyboard = new Keyboard();

    // Function defined at top of file; creates supplementary pairs for extended Unicode codepoints.
    var u = toSupplementaryPairString;

    var STORES = [
      {smp: false, in: "apple", out: ['a','p','p','l','e']},
      //In JS-escaped form:  "\\ud804\\udd12\\ud804\\udd0d\\ud804\\udd0f\\ud804\\udd10\\ud804\\udd0a\\ud804\\udd05"
      //(Supplementary pairs, copied from the easy_chakma keyboard.)
      {smp: true, in: "𑄒𑄍𑄏𑄐𑄊𑄅", out: ["𑄒","𑄍","𑄏","𑄐","𑄊","𑄅"]},
      // Built in-line via function.  Looks functionally equivalent to "apple", but with SMP characters.
      {smp: true, in: (u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be)),
        out: [u(0x1d5ba), u(0x1d5c9), u(0x1d5c9), u(0x1d5c5), u(0x1d5be)]}
    ];

    for(var i=0; i < STORES.length; i++) {
      var s = STORES[i];

      String.kmwEnableSupplementaryPlane(s.smp);
      var result = processor.keyboardInterface._ExplodeStore(s.in);
      assert.sameOrderedMembers(result, s.out, "Failure exploding " + (s.smp ? "SMP" : "non-SMP") + " string value \"" + s.in + "\"");
    }
    String.kmwEnableSupplementaryPlane(false);
  });

  // TODO:  Migrate a Node-based copy of the variable store tests here as well.
});