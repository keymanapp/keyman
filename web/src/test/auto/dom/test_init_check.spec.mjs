// A simple "does device detection work properly" unit test.
// We'll let things break after this reports, since this will likely signal a LOT of other failures.
import { assert } from 'chai';

import Device from 'keyman/engine/device-detect';

/* Note - we still have to prevent errors setting up test resources;
 * Karma will fail to report errors for affected browsers otherwise.
 *
 * This simply gives us a test report with the error that can be shown
 * if we disable other tests when this fails.
 */
describe('Test Initialization', function() {
  // We can't use testconfig timeouts yet (they rely on test success), so we simply give
  // this should-be-simple test a nice, large block of time to run.
  this.timeout(15000);

  it("Detects device without JS errors", function() {
    var device;
    try {
      device = new Device();
      device.detect();

      console.log("Detected platform: " + device.browser + " on " + device.OS + " with form factor " + device.formFactor);
    } catch (err) {
      console.error("Error during device detection: " + err.toString());
      assert.fail(err);
    }
  });
});