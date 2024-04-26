import { assert } from '/node_modules/chai/chai.js';

import {
  loadKeyboardFromJSON,
  runKeyboardTestFromJSON,
  setupKMW,
  teardownKMW
} from "../test_utils.js";

describe('Engine - Chirality', function() {
  this.timeout(testconfig.timeouts.scriptLoad);

  before(function() {
    fixture.setBase('fixtures');
    return setupKMW(null, testconfig.timeouts.scriptLoad);
  });

  beforeEach(function(done) {
    fixture.load("singleInput.html");

    window.setTimeout(function() {
      done()
    }, testconfig.timeouts.eventDelay);
  });

  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });

  it('Keyboard + OSK simulation', async function() {
    this.timeout(testconfig.timeouts.scriptLoad * (testconfig.mobile ? 1 : 2));
    /* Interestingly, this still works on iOS, probably because we're able to force-set
     * the 'location' property in the simulated event on mobile devices, even when iOS neglects to
     * set it for real events.
     */
    return await runKeyboardTestFromJSON('/engine_tests/chirality.json',
                            {usingOSK: false},
                            assert.equal,
                            testconfig.timeouts.scriptLoad).then(async () => {
      /* We only really care to test the 'desktop' OSK because of how it directly models the modifier keys.
        *
        * The 'phone' and 'layout' versions take shortcuts that bypass any tricky chiral logic;
        * a better test for those would be to ensure the touch OSK is constructed properly.
        */
      if(!testconfig.mobile) {
        return await runKeyboardTestFromJSON('/engine_tests/chirality.json', {usingOSK: true}, assert.equal, testconfig.timeouts.scriptLoad);
      }
    });
  });
});