import * as KeymanOSK from '/@keymanapp/keyman/build/engine/osk/lib/index.mjs';
import Device from '/@keymanapp/keyman/build/engine/device-detect/lib/index.mjs';

import { loadKeyboardsFromStubs } from '../../kbdLoader.mjs';
import { timedPromise } from '/@keymanapp/web-utils/build/lib/index.mjs';

import { assert } from '/node_modules/chai/chai.js';

const device = new Device();
device.detect();

const TestResources = {
  OskConfig: {
    isEmbedded: false,
    pathConfig: {
      fonts: '',
      resources: '/@keymanapp/keyman/src/resources'
    },
    hostDevice: device.coreSpec,
    allowHideAnimations: false // shortens timings.
  }
}

describe('OSK activation', function () {
  this.timeout(__karma__.config.args.find((arg) => arg.type == "timeouts").standard);

  before(async () => {
    const stubs = [__json__['/keyboards/khmer_angkor']];
    TestResources.Keyboards = await loadKeyboardsFromStubs(stubs, '/');
  });

  beforeEach(() => {
    fixture.set('<div id="osk-container" style="width: fit-content"></div>');
  });

  afterEach(() => {
    fixture.cleanup();
  });

  it('InlinedOSK - standard case', async () => {
    const container = document.getElementById('osk-container');

    let osk = new KeymanOSK.InlinedOSKView(TestResources.OskConfig);

    osk.setSize(600, 400);
    container.appendChild(osk.element);

    osk.activeKeyboard = TestResources.Keyboards['khmer_angkor'];

    // Needs a moment to actually load the stylesheets once visible - which occurs once first displayed.
    await timedPromise(100);

    // Assert the 'container' element's size matches what we set the OSK to.
    assert.equal(container.offsetWidth, 600);
    assert.equal(container.offsetHeight, 400);

    assert.equal(osk.mayHide(false), true);

    osk.activationModel.enabled = false;

    // Allow the underlying message / Promise loop to clear out before proceeding; there's
    // an insta-resolving Promise (from osk.startHide) involved here.
    await timedPromise(0);

    assert.equal(container.offsetWidth, 0);
    assert.equal(container.offsetHeight, 0);
  });


  it('InlinedOSK - static case', async () => {
    // Static case - does not allow hide operations.
    // In fact, assigning to the `.enabled` property of the activator causes a run-time error!
    // So, make sure we don't make unexpected errors.
    const container = document.getElementById('osk-container');

    let osk = new KeymanOSK.InlinedOSKView({
      ...TestResources.OskConfig,
      activator: new KeymanOSK.StaticActivator()
    });

    osk.setSize(600, 400);
    container.appendChild(osk.element);

    osk.activeKeyboard = TestResources.Keyboards['khmer_angkor'];

    // Needs a moment to actually load the stylesheets once visible - which occurs once first displayed.
    await timedPromise(100);

    // Assert the 'container' element's size matches what we set the OSK to.
    assert.equal(container.offsetWidth, 600);
    assert.equal(container.offsetHeight, 400);

    try {
      osk.activationModel.enabled = false;
      assert.fail("Assignment to get-only .enabled property should not succeed!");
    } catch (err) {
      // An error should have been thrown - .enabled is an unsettable property here.
    }

    assert.equal(osk.mayHide(false), false);

    // Must not throw an error!
    osk.startHide(true);

    // Allow the underlying message / Promise loop to clear out before proceeding; there's
    // an insta-resolving Promise (from osk.startHide) involved here.
    await timedPromise(0);

    /*
     * Possible future enhancement note for the above setup:
     * https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link#stylesheet_load_events
     * We could set up a system to note when stylesheets load & set up a Promise for this...
     * but it gets tricky b/c of a few things:
     *
     * - "The load event fires ... immmediately before the styles start being applied."
     *   - We want 'after'.
     * - If a stylesheet's been loaded once already, does the load event fire again when reapplied?
     *   - If not, we'll need to note things that were loaded once & add rigging to the Promise
     *     setup accordingly.
     */

    // Is unhideable.
    assert.equal(container.offsetWidth, 600);
    assert.equal(container.offsetHeight, 400);
  });
});