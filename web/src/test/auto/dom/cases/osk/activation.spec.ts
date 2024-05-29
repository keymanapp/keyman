import * as KeymanOSK from 'keyman/engine/osk';
import Device from 'keyman/engine/device-detect';

import { loadKeyboardsFromStubs } from '../../kbdLoader.js';
import { timedPromise } from '@keymanapp/web-utils';
import { type Keyboard } from '@keymanapp/keyboard-processor';

import { assert } from 'chai';

const device = new Device();
device.detect();

const TestResources = {
  OskConfig: {
    isEmbedded: false,
    pathConfig: {
      fonts: '',
      resources: '/web/build/publish/debug/'
    },
    hostDevice: device.coreSpec,
    allowHideAnimations: false // shortens timings.
  },
  Keyboards: null as Awaited<ReturnType<typeof loadKeyboardsFromStubs>>
}

const host = document.createElement('div');
document.body.appendChild(host);

describe('OSK activation', function () {
  this.timeout(5000);

  before(async () => {
    const fixture = await fetch('resources/stubs/khmer_angkor.json');
    const stubJSON = await fixture.json();
    TestResources.Keyboards = await loadKeyboardsFromStubs([stubJSON], '/');
  });

  beforeEach(() => {
    host.innerHTML = '<div id="osk-container" style="width: fit-content"></div>';
  });

  afterEach(() => {
    host.innerHTML = '';
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

    // @ts-ignore // mayHide is protected.
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

    // @ts-ignore // mayHide is protected.
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