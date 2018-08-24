module.exports = function(config) {
  var base = require("./base.conf.js");

  var timeouts = base.client.args[0];
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
   * Definition of browser sets possibly relevant for testing.
   */
  var CURRENT_MAC_LAUNCHERS = {
    bs_firefox_mac: {
      browser: 'firefox',
      browser_version: '58',
      os: 'OS X',
      os_version: 'High Sierra'
    },
    bs_safari_mac: {
      browser: 'safari',
      browser_version: '11.1',
      os: 'OS X',
      os_version: 'High Sierra'
    },
    bs_chrome_mac: {
      browser: 'chrome',
      browser_version: '64.0',
      os: 'OS X',
      os_version: 'High Sierra'
    }
  };

  
  var CURRENT_IOS_LAUNCHERS = {
    bs_iphoneX: {
      device: 'iPhone 8 Plus', // Ideally, we'd use 'iPhone X', but BrowserStack's version is being problematic lately.
      real_mobile: true,
      os: 'ios',
      os_version: '11.0'
    },
    bs_ipad5: {
      device: 'iPad 5th',
      real_mobile: true,
      os: 'ios',
      os_version: '11.0'
    }
  };

  var CURRENT_WIN_LAUNCHERS = {
    bs_firefox_win: {
      os: 'Windows',
      os_version: '10',
      browser: 'firefox',
      browser_version: '58.0'
    },
    bs_chrome_win: {
      os: 'Windows',
      os_version: '10',
      browser: 'chrome',
      browser_version: '64.0'
    },
    bs_ie_win: {
      os: 'Windows',
      os_version: '10',
      browser: 'ie',
      browser_version: '11.0'
    },
    bs_edge_win: {
      os: 'Windows',
      os_version: '10',
      browser: 'edge',
      browser_version: '16.0'
    }
  }

  var CURRENT_ANDROID_LAUNCHERS = {
    bs_native_android: {
      os: 'android',
      os_version: '7.1',
      browser: 'android',
      real_mobile: true,
      device: 'Samsung Galaxy Note 8'
    },
    bs_chrome_android: {
      os: 'android',
      os_version: '7.1',
      browser: 'chrome',
      real_mobile: true,
      device: 'Samsung Galaxy Note 8'
    }
  }

  // Sadly, legacy IE isn't very testable with Mocha.  One of its dependencies requires a feature that is IE11+.

  /*
   * Final selection of the sets to be used for BrowserStack testing.
   */
  var FINAL_LAUNCHER_DEFS = mergeLaunchers( CURRENT_ANDROID_LAUNCHERS,
                                            CURRENT_IOS_LAUNCHERS,
                                            CURRENT_WIN_LAUNCHERS,
                                            CURRENT_MAC_LAUNCHERS);

  var FINAL_BROWSER_LIST = toBrowserList(FINAL_LAUNCHER_DEFS);

  /*
   * Final definition of our BrowserStack testing Karma configuration.
   */

  var specifics = {
    // BrowserStack configuration options
    browserStack: {
      video: false,
      browserDisconnectTimeout: 3e5,
      retryLimit: 1, // 0 is ignored.
      startTunnel: true,
    },

    // Attempts to avoid generating a 'fail' exit code if one of our selected browsers on BrowserStack goes poof.
    failOnEmptyTestSuite: false,

    captureTimeout: 6e5, // in milliseconds
    browserNoActivityTimeout: 3e5,
    browserDisconnectTimeout: 3e5,
    browserDisconnectTolerance: 3,

    // Avoids generating a 'fail' exit code if one of our selected browsers on BrowserStack goes poof.
    failOnEmptyTestSuite: false,

    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['teamcity'],

    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,

    // Concurrency level
    // For CI, it really helps to keep a nice, clean set of output logs.
    concurrency: 5,

    customLaunchers: FINAL_LAUNCHER_DEFS,

    browsers: FINAL_BROWSER_LIST
  };

  config.set(Object.assign(specifics, base));
}
