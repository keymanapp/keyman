import { assert } from 'chai';
import sinon from 'sinon';

import {
  FixtureLayoutConfiguration,
  HostFixtureLayoutController,
  InputSequenceSimulator
} from '#tools';

import { DEFAULT_BROWSER_TIMEOUT } from '@keymanapp/common-test-resources/test-timeouts.mjs';

describe("'Canary' checks", function() {
  this.timeout(DEFAULT_BROWSER_TIMEOUT);

  let domain: string;

  before(async () => {
    let loc = document.location;
    // config.testFile generally starts with a '/', with the path resembling the actual full local
    // filesystem for the drive.
    domain = `${loc.protocol}/${loc.host}`

    // Test-config setups will take care of the rest; the server-path will be rooted at the repo root.
    // With aliasing for resources/.
  });

  it('host-fixture.html + gestureHost.css', function() {
    let element = document.getElementById('host-fixture');

    // Ensure that not only did we get an element, we got the expected element.
    assert.isNotNull(element);
    assert.isDefined(element);
    // If the CSS is missing, the element will default to zero height.
    assert.notEqual(element.getBoundingClientRect().height, 0);
  });

  it('canaryRecording.json', async function() {
    const jsonResponse = await fetch(new URL(`${domain}/resources/json/canaryRecording.json`));
    const jsonObject = await jsonResponse.json();

    assert.isNotNull(jsonObject);
    assert.isDefined(jsonObject);

    let config = new FixtureLayoutConfiguration(jsonObject.config);
    assert.equal(config.deviceStyle, 'screen4');
  });

  it('Testing.HostFixtureLayoutController', async function() {
    const jsonResponse = await fetch(new URL(`${domain}/resources/json/canaryRecording.json`));
    const jsonObject = await jsonResponse.json();

    const targetRoot = document.getElementById('host-fixture');
    let controller = new HostFixtureLayoutController();
    // Note:  this is set BEFORE the controller is configured (in the following line).
    // The class is designed to support this.
    controller.layoutConfiguration = new FixtureLayoutConfiguration(jsonObject.config);

    try {
      await controller.connect();
      assert.isTrue(targetRoot.className.indexOf('screen4') > -1, "Could not apply configuration spec from recorded JSON!");
    } finally {
      controller.destroy();
    }
  })

  describe('event simulation', function() {
    beforeEach(function(done) {
      this.controller = new HostFixtureLayoutController();
      this.controller.connect().then(() => done());
    });

    afterEach(function() {
      this.controller.destroy();
    });

    it("InputSequenceSimulator.replayTouchSample", async function() {
      let playbackEngine = new InputSequenceSimulator(this.controller);
      let layout = new FixtureLayoutConfiguration("screen2", "bounds1", "full", "safe-loose");
      this.controller.layoutConfiguration = layout;

      let fireEvent = () => {
        playbackEngine.replayTouchSamples(/*relative coord:*/ [ { sample: {targetX: 10, targetY: 10}, identifier: 1}],
                                          /*state:*/         "start",
                                          /*recentTouches:*/  [],
                                        );
      }

      // Ensure that the expected handler is called.
      let fakeHandler = sinon.fake();
      this.controller.recognizer.on('inputstart', fakeHandler);
      fireEvent();

      await new Promise((resolve) => {
        window.setTimeout(resolve, 0);
      }).then(() => new Promise((resolve) => {
        window.setTimeout(resolve, 0);
      }));

      assert.isTrue(fakeHandler.called, "Unit test attempt failed:  handler was not called successfully.");
    });

    it("InputSequenceSimulator.replayMouseSample", async function() {
      let playbackEngine = new InputSequenceSimulator(this.controller);
      let layout = new FixtureLayoutConfiguration("screen2", "bounds1", "full", "safe-loose");
      this.controller.layoutConfiguration = layout;

      let fireEvent = () => {
        playbackEngine.replayMouseSample(/*relative coord:*/ {targetX: 15, targetY: 15},
                                          /*state:*/         "start"
                                        );
      }

      // Ensure that the expected handler is called.
      let fakeHandler = sinon.fake();
      this.controller.recognizer.on('inputstart', fakeHandler);
      fireEvent();

      await new Promise((resolve) => {
        window.setTimeout(resolve, 0);
      });

      assert.isTrue(fakeHandler.called, "Unit test attempt failed:  handler was not called successfully.");
    });
  });
});