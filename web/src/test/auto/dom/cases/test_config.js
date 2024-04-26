import { assert } from '/node_modules/chai/chai.js';

/*
 * A canary test to ensure our timeout configurations are working correctly for our CI.
 */
describe('Test configuration', function () {
  this.timeout(testconfig.timeouts.standard);

  it('Correctly retrieves timeout configurations', function() {
    let configArgs = __karma__.config.args;
    assert.isNotNull(configArgs, "Cannot find the custom arguments object from our Karma config.");

    var timeoutConfig;

    for(var i=0; i < configArgs.length; i++) {
      if(configArgs[i].type == 'timeouts') {
        timeoutConfig = configArgs[i];
      }
    }

    assert.isNotNull(timeoutConfig, "Cannot find the timeout configuration object.");

    // Now, were they processed correctly?
    assert.isNotNull(window['testconfig'], "Test global 'config' is missing!");
    let timeouts = testconfig.timeouts;
    // May be longer if an extra factor (such as a mobile device multiplier) is added.
    assert.isAtLeast(timeouts.standard, timeoutConfig.standard, "Timeouts were not parsed properly for test configuration.");
  })
});