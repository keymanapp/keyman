var assert = chai.assert;

/*
 * A canary test to ensure our timeout configurations are working correctly for our CI.
 */
describe('Test configuration', function () {
  this.timeout(config.timeouts.standard);

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
    assert.isNotNull(window['config'], "Test global 'config' is missing!");
    let timeouts = config.timeouts;
    assert.equal(timeouts.standard, timeoutConfig.standard, "Timeouts were not parsed properly for test configuration.");
  })
});