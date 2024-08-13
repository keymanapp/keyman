/**
 * Defines our 'primary' set of browser configurations to unit-test against via BrowserStack.
 */
var PRIMARY_LAUNCHERS = {
  bs_safari_mac_m: {
    browser: 'safari',
    browser_version: '17.3',
    os: 'OS X',
    os_version: 'Sonoma'
  },
  bs_chrome_win: {
    os: 'Windows',
    os_version: '10',
    browser: 'chrome',
    browser_version: '100.0'
  },
  bs_chrome_android: {
    os: 'android',
    os_version: '10.0',
    browser: 'chrome',
    real_mobile: true,
    device: 'Samsung Galaxy Note 20'
  }
}

/**
 * Defines our 'secondary' set of browser configurations to unit-test against via BrowserStack.
 */
var SECONDARY_LAUNCHERS = {
  bs_firefox_mac: {
    browser: 'firefox',
    browser_version: '100',
    os: 'OS X',
    os_version: 'Monterey'
  },
  bs_chrome_mac: {
    browser: 'chrome',
    browser_version: '100',
    os: 'OS X',
    os_version: 'Monterey'
  },
  bs_firefox_win: {
    os: 'Windows',
    os_version: '10',
    browser: 'firefox',
    browser_version: '100'
  }
}

/* --- End of test environment definitions. --- */

/**
 * Provides an exportable function for configuring a Karma setup for execution in our CI processes
 * via BrowserStack.
 *
 * @param {*} baseConfigParams   The project's base configuration parameters, independent of local vs CI test mode
 * @param {*} targetSet          Either "partial" or "full":  "partial" aims for a smaller yet representative
 *                               subset of target browser/device pairings.  Defaults to "full" if unspecified.
 * @returns {(config) => void}   The function to be exported in the final Karma configuration file
 */
module.exports = function(baseConfigParams  /* the project's base configuration */,
                          targetSet         /* a 'flag' for whether to use a large set of test targets */) {
  var timeouts = baseConfigParams.client.args[0];
  var browserStackModifier = 10;

  for(key in timeouts) {
    if(typeof timeouts[key] == 'number') {
      timeouts[key] = timeouts[key] * browserStackModifier;
    }
  }

  timeouts.mobileFactor = 2; // Extra timeout padding for running on a remote mobile device.

  /*
   * Definition of utility functions for managing our browser lists.
   */
  var mergeLaunchers = function() {
    var mergedDefs = {};
    var i;
    for(i=0; i < arguments.length; i++) {
      for(var name in arguments[i]) {
        mergedDefs[name] = arguments[i][name];

        // Necessary for Karma to process it properly.
        mergedDefs[name].base = 'BrowserStack';
      }
    }

    return mergedDefs;
  }

  var toBrowserList = function(mergedSet) {
    var list = [];
    for(var name in mergedSet) {
      list.push(name);
    }

    return list;
  }

  /*
   * Final selection of the sets to be used for BrowserStack testing.
   */

  var FINAL_LAUNCHER_DEFS;
  if(targetSet == 'full') {
    FINAL_LAUNCHER_DEFS = mergeLaunchers( PRIMARY_LAUNCHERS,
                                          SECONDARY_LAUNCHERS);
  } else {
    // The function also does some necessary bookkeeping that makes for
    // cleaner definitions at the file's top, so we call it even if we
    // aren't merging lists.
    FINAL_LAUNCHER_DEFS = mergeLaunchers(PRIMARY_LAUNCHERS);
  }

  var FINAL_BROWSER_LIST = toBrowserList(FINAL_LAUNCHER_DEFS);

  /*
   * Final definition of our BrowserStack testing Karma configuration.
   */

  var specifics = {
    // BrowserStack configuration options
    browserStack: {
      video: true,
      browserDisconnectTimeout: 6e4, // 1 minute (60s => 60,000ms)
      retryLimit: 3, // 0 is ignored.
      startTunnel: true,
    },

    captureTimeout: 1.2e5, // in milliseconds
    browserNoActivityTimeout: 6e4,
    browserDisconnectTimeout: 6e4,
    browserDisconnectTolerance: 3,

    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['teamcity', 'BrowserStack'],

    // Concurrency level
    // We are alloted 5 total at once from BrowserStack.  That said, note that we have multiple build configs that may
    // need BrowserStack-based testing simultaneously.  It may be best to avoid bottlenecking on that limitation.
    concurrency: 1,

    customLaunchers: FINAL_LAUNCHER_DEFS,

    browsers: FINAL_BROWSER_LIST
  };

  // The core function that the final config file needs to provide.
  return function(config) {
    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    specifics.logLevel = config.LOG_DEBUG;

    config.set(Object.assign(specifics, baseConfigParams));
  }
}
