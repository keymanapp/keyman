import { assert } from 'chai';

import {
  runKeyboardTestFromJSON,
  setupKMW,
  teardownKMW
} from "../test_utils.js";

const baseTimeout = 5000;

const host = document.createElement('div');
document.body.appendChild(host);

describe('Engine - Chirality', function() {
  this.timeout(baseTimeout);

  before(function() {
    return setupKMW(null, baseTimeout);
  });

  beforeEach(async function() {
    const singleton = document.createElement('input');
    singleton.id = 'singleton';
    host.appendChild(singleton);
  });

  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    host.innerHTML = '';
  });

  it('Keyboard + OSK simulation', async function() {
    /* Interestingly, this still works on iOS, probably because we're able to force-set
     * the 'location' property in the simulated event on mobile devices, even when iOS neglects to
     * set it for real events.
     */
    return await runKeyboardTestFromJSON('resources/json/engine_tests/chirality.json',
                            {usingOSK: false},
                            assert.equal,
                            baseTimeout).then(async () => {
      /* We only really care to test the 'desktop' OSK because of how it directly models the modifier keys.
        *
        * The 'phone' and 'layout' versions take shortcuts that bypass any tricky chiral logic;
        * a better test for those would be to ensure the touch OSK is constructed properly.
        */
      return await runKeyboardTestFromJSON('resources/json/engine_tests/chirality.json', {usingOSK: true}, assert.equal, baseTimeout);
    });
  });
});