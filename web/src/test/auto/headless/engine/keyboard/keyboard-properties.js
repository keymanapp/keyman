import { assert } from 'chai';
import fs from 'fs';
import path from 'path';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { KeyboardProperties, SpacebarText } from 'keyman/engine/keyboard';

describe('Keyboard Properties', function() {
  let commonResourcesPackage = '@keymanapp/common-test-resources';
  let commonStubsSubpath = 'json/keyboards';
  let rootCommonStubPath = `${commonResourcesPackage}/${commonStubsSubpath}`;

  it('initialization from KMW\'s addKeyboards() API spec', () => {
    // require.resolve cannot resolve a directory directly, unfortunately.
    // Needs a file to 'root' the resolution mechanism.  See https://github.com/nodejs/node/issues/42219.
    const resolvedResourcesPath = path.dirname(require.resolve(`${commonResourcesPackage}/index.mjs`));
    const resolvedCommonStubPath = `${resolvedResourcesPath}/${commonStubsSubpath}`;
    let files = fs.readdirSync(resolvedCommonStubPath);

    for(let file of files) {
      let stub = JSON.parse(fs.readFileSync(`${resolvedCommonStubPath}/${file}`));

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
    let stub = JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/khmer_angkor.json`)));
    let propObject = new KeyboardProperties(stub);

    // Without a configured SpacebarText value, will display the keyboard name.
    assert.equal(propObject.displayName, propObject.name);

    KeyboardProperties.spacebarTextMode = SpacebarText.BLANK;
    assert.equal(propObject.displayName, '');

    KeyboardProperties.spacebarTextMode = SpacebarText.KEYBOARD;
    assert.equal(propObject.displayName, propObject.name);

    KeyboardProperties.spacebarTextMode = SpacebarText.LANGUAGE;
    assert.equal(propObject.displayName, propObject.langName);

    KeyboardProperties.spacebarTextMode = SpacebarText.LANGUAGE_KEYBOARD;
    assert.isTrue(propObject.displayName.includes(propObject.langName) && propObject.displayName.includes(propObject.name));
  });

  it('does not override directly-specified display-name text', () => {
    // Could convert to run on all stubs, but... this should be fine as-is.
    let stub = JSON.parse(fs.readFileSync(require.resolve(`${rootCommonStubPath}/khmer_angkor.json`)));

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