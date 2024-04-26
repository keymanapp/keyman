import { assert } from '../../../../../../../../node_modules/chai/chai.js';

import {
  FixtureLayoutConfiguration,
  HostFixtureLayoutController,
  InputSequenceSimulator
} from '../../../../../build/tools/lib/index.mjs';

describe("'Canary' checks", function() {
  this.timeout(testconfig.timeouts.standard);

  before(function() {
    fixture.setBase('');
  })

  it('host-fixture.html + gestureHost.css', function() {
    let element = fixture.load('host-fixture.html')[0];

    // Ensure that not only did we get an element, we got the expected element.
    assert.isNotNull(element);
    assert.isDefined(element);
    assert.equal(element.id, 'host-fixture');
    // If the CSS is missing, the element will default to zero height.
    assert.notEqual(element.getBoundingClientRect().height, 0);
  });

  it('canaryRecording.json', function() {
    let jsonObject = window['__json__'].canaryRecording;

    assert.isNotNull(jsonObject);
    assert.isDefined(jsonObject);

    let config = new FixtureLayoutConfiguration(jsonObject.config);
    assert.equal(config.deviceStyle, 'screen4');
  });

  it('Testing.HostFixtureLayoutController', function(done) {
    let targetRoot = fixture.load('host-fixture.html')[0];
    let jsonObject = window['__json__'].canaryRecording;

    let controller = new HostFixtureLayoutController();
    // Note:  this is set BEFORE the controller is configured (in the following line).
    // The class is designed to support this.
    controller.layoutConfiguration = new FixtureLayoutConfiguration(jsonObject.config);
    controller.connect().then(() => {
      assert.isTrue(targetRoot.className.indexOf('screen4') > -1, "Could not apply configuration spec from recorded JSON!");
      done();
    }).finally(() => controller.destroy());
  })

  afterEach(function() {
    fixture.cleanup();
  })

  describe('event simulation', function() {
    beforeEach(function(done) {
      fixture.load('host-fixture.html');
      this.controller = new HostFixtureLayoutController();
      this.controller.connect().then(() => done());
    });

    afterEach(function() {
      this.controller.destroy();
      fixture.cleanup();
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