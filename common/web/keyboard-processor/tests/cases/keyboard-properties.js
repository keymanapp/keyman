import { assert } from 'chai';
import fs from 'fs';
import vm from 'vm';

import KeyboardProperties from '@keymanapp/keyboard-processor/build/obj/keyboards/keyboardProperties.js';
import SpacebarText from '@keymanapp/keyboard-processor/build/obj/keyboards/spacebarText.js';

describe('Keyboard Properties', function() {
  let rootCommonStubPath = '../../test/resources/json/keyboards/';

  it('initialization from KMW\'s addKeyboards() API spec', () => {
    let files = fs.readdirSync(rootCommonStubPath);

    for(let file of files) {
      let stub = JSON.parse(fs.readFileSync(`${rootCommonStubPath}/${file}`));

      let dataset = [];
      if(stub.languages instanceof Array) {
        dataset = KeyboardProperties.fromMultilanguageAPIStub(stub);
      } else {
        dataset = [new KeyboardProperties(stub)];
      }

      // do verification
      for(let data of dataset) {
        assert.isOk(data.id);
        assert.isOk(data.name);
        assert.isOk(data.langId);
        assert.isOk(data.langName);
        // Do not make assertions on font, oskFont - those may be undefined for some keyboards.

        // Generated dynamically from the backing source data.
        assert.isOk(data.displayName);
      }
    }
  });

  it('generates display-name text if not directly-specified', () => {
    // Could convert to run on all stubs, but... this should be fine as-is.
    let stub = JSON.parse(fs.readFileSync(`${rootCommonStubPath}/khmer_angkor.json`));
    let propObject = new KeyboardProperties(stub);

    // Without a configured SpacebarText value, will display the keyboard name.
    assert.equal(propObject.displayName, propObject.name);

    propObject.spacebarTextMode = SpacebarText.BLANK;
    assert.equal(propObject.displayName, '');

    propObject.spacebarTextMode = SpacebarText.KEYBOARD;
    assert.equal(propObject.displayName, propObject.name);

    propObject.spacebarTextMode = SpacebarText.LANGUAGE;
    assert.equal(propObject.displayName, propObject.langName);

    propObject.spacebarTextMode = SpacebarText.LANGUAGE_KEYBOARD;
    assert.isTrue(propObject.displayName.includes(propObject.langName) && propObject.displayName.includes(propObject.name));
  });

  it('does not override directly-specified display-name text', () => {
    // Could convert to run on all stubs, but... this should be fine as-is.
    let stub = JSON.parse(fs.readFileSync(`${rootCommonStubPath}/khmer_angkor.json`));

    const customDisplayName = "(custom)";
    let propObject = new KeyboardProperties(stub);

    propObject.displayName = customDisplayName;

    // Without a configured SpacebarText value, will display the keyboard name.
    assert.equal(propObject.displayName, customDisplayName);

    propObject.spacebarTextMode = SpacebarText.BLANK;
    assert.equal(propObject.displayName, customDisplayName);

    propObject.spacebarTextMode = SpacebarText.KEYBOARD;
    assert.equal(propObject.displayName, customDisplayName);

    propObject.spacebarTextMode = SpacebarText.LANGUAGE;
    assert.equal(propObject.displayName, customDisplayName);

    propObject.spacebarTextMode = SpacebarText.LANGUAGE_KEYBOARD;
    assert.equal(propObject.displayName, customDisplayName);

    propObject.displayName = null; // clear the value
    assert.notEqual(propObject.displayName, customDisplayName);
    assert.isTrue(propObject.displayName.includes(propObject.langName) && propObject.displayName.includes(propObject.name));

    propObject.displayName = customDisplayName;
    assert.equal(propObject.displayName, customDisplayName);
  });
});