import * as KeymanOSK from 'keyman/engine/osk';
import Device from 'keyman/engine/device-detect';

import { loadKeyboardsFromStubs } from '../../kbdLoader.js';
import { timedPromise } from '@keymanapp/web-utils';
import { type Keyboard } from '@keymanapp/keyboard-processor';

import sinon from 'sinon';

import { assert } from 'chai';

const device = new Device();
device.detect();

const TestResources = {
  OskConfig: {
    isEmbedded: false,
    pathConfig: {
      fonts: '',
      resources: '/web/build/publish/debug'
    },
    hostDevice: device.coreSpec,
    allowHideAnimations: false // shortens timings.
  },
  Keyboards: {} as Awaited<ReturnType<typeof loadKeyboardsFromStubs>>
}

const host = document.createElement('div');
document.body.appendChild(host);

describe('OSK events', function () {
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

  it('InlinedOSK - onHide / onShow', async () => {
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

    // Setup complete.

    // Hide the OSK + check for related event
    let legacyHideStub = sinon.fake();
    osk.legacyEvents.addEventListener('hide', legacyHideStub);

    osk.activationModel.enabled = false;
    assert.isTrue(legacyHideStub.calledOnce);

    // The hide actually occurs on a Promise's completion.
    // Adding the 'await' makes this clearer during test maintenance.
    await Promise.resolve();

    // Show the OSK + check for related event
    let legacyShowStub = sinon.fake();
    osk.legacyEvents.addEventListener('show', legacyShowStub);

    osk.activationModel.enabled = true;
    assert.isTrue(legacyShowStub.calledOnce);
  });
});