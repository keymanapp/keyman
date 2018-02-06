// Karma configuration
// Generated on Mon Jan 29 2018 11:58:53 GMT+0700 (SE Asia Standard Time)

// Super-useful!  http://www.bradoncode.com/blog/2015/02/27/karma-tutorial/#handling-html-fixtures

module.exports = function(config) {

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
      browser_version: '11.0',
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
      device: 'iPhone X',
      real_mobile: false,
      os: 'ios',
      os_version: '11.0'
    },
    bs_ipad5: {
      device: 'iPad 5th',
      real_mobile: false,
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

  var LEGACY_MAC_LAUNCHERS = {
    bs_firefox_legacy_mac: {
      browser: 'firefox',
      browser_version: '40.0',
      os: 'OS X',
      os_version: 'Mountain Lion'
    },
    bs_safari_legacy_mac: {
      browser: 'safari',
      browser_version: '6.2',
      os: 'OS X',
      os_version: 'Mountain Lion'
    }
  };

  var LEGACY_IOS_LAUNCHERS = {
    bs_iphone7: {
      device: 'iPhone 7',
      real_mobile: false,
      os: 'ios',
      os_version: '10.3'
    }
  };

  // Sadly, legacy IE isn't very testable with Mocha.  One of its dependencies requires a feature that is IE11+.

  /*
   * Final selection of the sets to be used for BrowserStack testing.
   */
  var FINAL_LAUNCHER_DEFS = mergeLaunchers( CURRENT_MAC_LAUNCHERS,
                                            CURRENT_IOS_LAUNCHERS,
                                            CURRENT_WIN_LAUNCHERS,
                                            CURRENT_ANDROID_LAUNCHERS,
                                            LEGACY_MAC_LAUNCHERS,
                                            LEGACY_IOS_LAUNCHERS);

  var FINAL_BROWSER_LIST = toBrowserList(FINAL_LAUNCHER_DEFS);

  /*
   * Final definition of our BrowserStack testing Karma configuration.
   */

  config.set({

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '..',

    // BrowserStack configuration options
    browserStack: {
      video: false,
      retryLimit: 1, // 0 is ignored.
      startTunnel: true,
    },

    captureTimeout: 180000, // in milliseconds

    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['mocha', 'chai', 'fixture'],


    // list of files / patterns to load in the browser
    files: [
      'unit_tests/test_utils.js', // A basic utility script useful for constructing tests
      'unit_tests/modernizr.js', // A dependency-managed utility script that helps with browser feature detection.
      'unit_tests/cases/**/*.js', // Where the tests actually reside.
      {pattern: 'release/unminified/web/**/*.css', watched: false, served: true, included: false}, // OSK resources
      {pattern: 'release/unminified/web/**/*.gif', watched: false, served: true, included: false}, // OSK resources
      {pattern: 'release/unminified/web/**/*.png', watched: false, served: true, included: false}, // OSK resources
      {pattern: 'release/unminified/web/*.js', watched: true, served: true, included: false},  // The actual KMW code.
      {pattern: 'release/unminified/web/*.map', watched: true, served: true, included: false}, // + sourcemaps.
      {pattern: 'unit_tests/fixtures/**/*.html', watched: true} // HTML structures useful for testing.
    ],

    proxies: {
      "/source/": "/base/release/unminified/web/"
    },


    // list of files / patterns to exclude
    exclude: [
    ],


    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
		'**/*.html'	: ['html2js']
    },


    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['teamcity'],

    // web server port
    port: 9876,


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,


    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true,

    // Continuous Integration mode
    // if true, Karma captures browsers, runs the tests and exits
	  // if false, it generates the pages and acts as a persistent server.
    singleRun: true,

    // Concurrency level
    // For CI, it really helps to keep a nice, clean set of output logs.
    concurrency: 5,

    customLaunchers: FINAL_LAUNCHER_DEFS,

    browsers: FINAL_BROWSER_LIST
  })
}
