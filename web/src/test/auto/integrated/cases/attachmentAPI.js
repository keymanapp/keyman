// var assert = chai.assert;

// describe('Attachment API', function() {
//   this.timeout(testconfig.timeouts.standard);

//   before(function() {
//     assert.isFalse(com.keyman.karma.DEVICE_DETECT_FAILURE, "Cannot run due to device detection failure.");
//     fixture.setBase('fixtures');

//     this.timeout(testconfig.timeouts.scriptLoad * 3);
//     return setupKMW({ attachType:'manual' }, testconfig.timeouts.scriptLoad).then(() => {
//       const kbd1 = loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", testconfig.timeouts.scriptLoad, { passive: true });
//       const kbd2 = loadKeyboardFromJSON("/keyboards/khmer_angkor.json",   testconfig.timeouts.scriptLoad, { passive: true });
//       return Promise.all([kbd1, kbd2]).then(() => {
//         return keyman.setActiveKeyboard("lao_2008_basic", "lo");
//       });
//     });
//   });

//   after(function() {
//     keyman.removeKeyboards('lao_2008_basic');
//     keyman.removeKeyboards('khmer_angkor');
//     teardownKMW();
//   });

//   beforeEach(function() {
//     fixture.load("robustAttachment.html");
//   });

//   afterEach(function(done) {
//     fixture.cleanup();
//     window.setTimeout(function(){
//       done();
//     }, testconfig.timeouts.eventDelay);
//   });

//   it("Enablement/Disablement", function(done) {
//     // Since we're in 'manual', we start detached.
//     var ele = document.getElementById(DynamicElements.addInput());
//     window.setTimeout(function() {
//       keyman.attachToControl(ele);
//       keyman.disableControl(ele);

//       // It appears that mobile devices do not instantly trigger the MutationObserver, so we need a small timeout
//       // for the change to take effect.
//       window.setTimeout(function() {
//         DynamicElements.assertAttached(ele);
//         let eventDriver = new KMWRecorder.BrowserDriver(ele);
//         eventDriver.simulateEvent(DynamicElements.keyCommand);
//         val = retrieveAndReset(ele);
//         assert.equal(val, DynamicElements.disabledOutput, "'Disabled' element performed keystroke processing!");

//         keyman.enableControl(ele);
//         window.setTimeout(function() {
//           DynamicElements.assertAttached(ele); // Happens in-line, since we directly request the attachment.
//           let eventDriver = new KMWRecorder.BrowserDriver(ele);
//           eventDriver.simulateEvent(DynamicElements.keyCommand);
//           val = retrieveAndReset(ele);
//           assert.equal(val, DynamicElements.enabledLaoOutput, "'Enabled' element did not perform keystroke processing!");
//           done();
//         }, testconfig.timeouts.eventDelay);
//       }, testconfig.timeouts.eventDelay);
//     }, testconfig.timeouts.eventDelay);
//   });
// });