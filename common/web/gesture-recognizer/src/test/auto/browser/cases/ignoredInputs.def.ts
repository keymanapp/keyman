import { assert } from 'chai';
import sinon from 'sinon';

import {
  FixtureLayoutConfiguration,
  HostFixtureLayoutController,
  InputSequenceSimulator,
  SequenceRecorder
} from '#tools';

describe("Layer one - DOM -> InputSequence", function() {
  this.timeout(5000);

  let controller: HostFixtureLayoutController;

  beforeEach(function(done) {
    controller = new HostFixtureLayoutController();
    controller.connect().then(() => done());
  });

  afterEach(function() {
    controller.destroy();
  });

  describe('other tests', function() {
    it("starts in roaming zone are ignored", function() {
      let playbackEngine = new InputSequenceSimulator(controller);
      let recorder = new SequenceRecorder(controller);
      let layout = new FixtureLayoutConfiguration("screen2", "bounds1", "full", "safe-loose");
      controller.layoutConfiguration = layout;

      let fireEvent = () => {
        playbackEngine.replayTouchSamples(/*relative coord:*/ [{sample: {targetX: 10, targetY: -5}, identifier: 1}],
                                          /*state:*/         "start",
                                          /*recentTouches:*/  [],
                                          /*targetElement:*/ controller.recognizer.config.maxRoamingBounds
                                        );
      }

      // This test is invalidated if the handler itself isn't called.  So... let's verify that!
      // This requires white-box inspection of the actual handler control-flow, and we must do
      let touchEngine = controller.recognizer.touchEngine;
      let trueHandler = touchEngine.onTouchStart;
      let fakeHandler = touchEngine.onTouchStart = sinon.fake();
      fireEvent();
      try {
        assert.isTrue(fakeHandler.called, "Unit test attempt failed:  handler was not called successfully.");
      } finally {
        // Restore the true implementation.
        touchEngine.onTouchStart = trueHandler;
      }

      // Now that we've put the true handler back in place, re-fire the event.  (We intercepted the
      // first 'fire' for the validation check above.)
      fireEvent();
      assert.equal(recorder.count, 0, "Input starting in roaming area was not ignored!");
    });

    it("ignores target-external events", function() {
      let playbackEngine = new InputSequenceSimulator(controller);
      let recorder = new SequenceRecorder(controller);
      let layout = new FixtureLayoutConfiguration("screen2", "bounds1", "full", "safe-loose");
      controller.layoutConfiguration = layout;

      let fireEvent = () => {
        playbackEngine.replayMouseSample(/*relative coord:*/ {targetX: -5, targetY: 15},
                                          /*state:*/         "start",
                                          /*targetElement:*/ document.body
                                        );
      }

      // This test is invalidated if the handler itself isn't called.  So... let's verify that!
      // Not quite covered by the canary cases b/c of the distinct targetElement.
      let mouseEngine = controller.recognizer.mouseEngine;
      let trueHandler = mouseEngine.onMouseStart;
      let fakeHandler = mouseEngine.onMouseStart = sinon.fake();
      fireEvent();
      try {
        assert.isTrue(fakeHandler.called, "Unit test attempt failed:  handler was not called successfully.");
      } finally {
        // Restore the true implementation.
        mouseEngine.onMouseStart = trueHandler;
      }

      // Now that we've put the true handler back in place, re-fire the event.  (We intercepted the
      // first 'fire' for the validation check above.)
      fireEvent();
      assert.equal(recorder.count, 0, "Input starting outside the main receiver element's hierarchy was not ignored!");
    });
  });
});