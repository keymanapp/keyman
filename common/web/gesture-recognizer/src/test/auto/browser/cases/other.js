// var assert = chai.assert;
// var expect = chai.expect;

// describe("Layer one - DOM -> InputSequence", function() {
//   this.timeout(testconfig.timeouts.standard);

//   before(function() {
//     fixture.setBase('');
//   });

//   beforeEach(function(done) {
//     fixture.load('host-fixture.html');
//     this.controller = new Testing.HostFixtureLayoutController();
//     this.controller.connect().then(() => done());
//   });

//   afterEach(function() {
//     this.controller.destroy();
//     fixture.cleanup();
//   });

//   describe('other tests', function() {
//     it("starts in roaming zone are ignored", function() {
//       let playbackEngine = new Testing.InputSequenceSimulator(this.controller);
//       let recorder = new Testing.SequenceRecorder(this.controller);
//       let layout = new Testing.FixtureLayoutConfiguration("screen2", "bounds1", "full", "safe-loose");
//       this.controller.layoutConfiguration = layout;

//       let fireEvent = () => {
//         playbackEngine.replayTouchSample(/*relative coord:*/ {targetX: 10, targetY: -5},
//                                           /*state:*/         "start",
//                                           /*identifier:*/    1,
//                                           /*otherTouches:*/  [],
//                                           /*targetElement:*/ this.controller.recognizer.config.maxRoamingBounds
//                                         );
//       }

//       // This test is invalidated if the handler itself isn't called.  So... let's verify that!
//       // Not quite covered by the canary cases b/c of the distinct targetElement.
//       let proto = com.keyman.osk.TouchEventEngine.prototype;
//       let trueHandler = proto.onTouchStart;
//       let fakeHandler = proto.onTouchStart = sinon.fake();
//       fireEvent();
//       try {
//         assert.isTrue(fakeHandler.called, "Unit test attempt failed:  handler was not called successfully.");
//       } finally {
//         // Restore the true implementation.
//         proto.onTouchStart = trueHandler;
//       }

//       fireEvent();
//       assert.equal(recorder.count, 0, "Input starting in roaming area was not ignored!");
//     });

//     it("ignores target-external events", function() {
//       let playbackEngine = new Testing.InputSequenceSimulator(this.controller);
//       let recorder = new Testing.SequenceRecorder(this.controller);
//       let layout = new Testing.FixtureLayoutConfiguration("screen2", "bounds1", "full", "safe-loose");
//       this.controller.layoutConfiguration = layout;

//       let fireEvent = () => {
//         playbackEngine.replayMouseSample(/*relative coord:*/ {targetX: -5, targetY: 15},
//                                           /*state:*/         "start",
//                                           /*targetElement:*/ document.body
//                                         );
//       }

//       // This test is invalidated if the handler itself isn't called.  So... let's verify that!
//       // Not quite covered by the canary cases b/c of the distinct targetElement.
//       let proto = com.keyman.osk.MouseEventEngine.prototype;
//       let trueHandler = proto.onMouseStart;
//       let fakeHandler = proto.onMouseStart = sinon.fake();
//       fireEvent();
//       try {
//         assert.isTrue(fakeHandler.called, "Unit test attempt failed:  handler was not called successfully.");
//       } finally {
//         // Restore the true implementation.
//         proto.onMouseStart = trueHandler;
//       }

//       fireEvent();
//       assert.equal(recorder.count, 0, "Input starting outside the main receiver element's hierarchy was not ignored!");
//     });
//   });
// });