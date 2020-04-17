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

        // Necessary for Karma to process our BrowserStack test environment configs properly.
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
      browser_version: '62',
      os: 'OS X',
      os_version: 'Mojave'
    },
    // Unfortunately, BrowserStack's Safari test clients have been quite unstable recently,
    // failing to even start running their tests.  We're disabling them until the issue goes away.

    // bs_safari_mac_hs: {
    //   browser: 'safari',
    //   browser_version: '11.1',
    //   os: 'OS X',
    //   os_version: 'High Sierra'
    // },
    // bs_safari_mac_m: {
    //   browser: 'safari',
    //   browser_version: '12',
    //   os: 'OS X',
    //   os_version: 'Mojave'
    // },
    bs_chrome_mac: {
      browser: 'chrome',
      browser_version: '70.0',
      os: 'OS X',
      os_version: 'Mojave'
    }
  };

  var CURRENT_WIN_LAUNCHERS = {
    // Currently, Firefox launcher is unstable; see https://github.com/karma-runner/karma-firefox-launcher/issues/93
    // (in particular "not maintained" commentary). 
    //bs_firefox_win: {
    //  os: 'Windows',
    //  os_version: '10',
    //  browser: 'firefox',
    //  browser_version: '62.0'
    //},
    bs_chrome_win: {
      os: 'Windows',
      os_version: '10',
      browser: 'chrome',
      browser_version: '70.0'
    },
    // On recent versions of Edge, launcher fails to start and/or stop Edge successfully
    //bs_edge_win: {
    //  os: 'Windows',
    //  os_version: '10',
    //  browser: 'edge',
    //  browser_version: '17.0'
    //}
  }

  var CURRENT_ANDROID_LAUNCHERS = {
    bs_native_android: {
      os: 'android',
      os_version: '7.1',
      browser: 'firefox',
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
   *
   * Native JavaScript testing on iOS devices is not supported by BrowserStack at this time.
   */
  var FINAL_LAUNCHER_DEFS = mergeLaunchers( CURRENT_ANDROID_LAUNCHERS,
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
