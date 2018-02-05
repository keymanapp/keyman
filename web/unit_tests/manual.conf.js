// Karma configuration
// Generated on Mon Jan 29 2018 11:58:53 GMT+0700 (SE Asia Standard Time)

// Super-useful!  http://www.bradoncode.com/blog/2015/02/27/karma-tutorial/#handling-html-fixtures

module.exports = function(config) {
  config.set({

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '..',


    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['mocha', 'chai', 'fixture'],


    // list of files / patterns to load in the browser
    files: [
      'unit_tests/test_utils.js', // A basic utility script useful for constructing tests
      'unit_tests/cases/**/*.js', // Where the tests actually reside.
      {pattern: 'release/unminified/web/**/*.css', watched: false, served: true, included: false}, // OSK resources
      {pattern: 'release/unminified/web/**/*.gif', watched: false, served: true, included: false}, // OSK resources
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
    reporters: ['mocha'],

    // web server port
    port: 9876,


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,


    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true,


    // start these browsers
    // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
    browsers: ['Firefox', 'IE', 'Chrome', 'Edge'], // Can be specified at run-time instead!
	// Future note for us:  https://www.npmjs.com/package/karma-browserstack-launcher


    // Continuous Integration mode
    // if true, Karma captures browsers, runs the tests and exits
	  // if false, it generates the pages and acts as a persistent server.
    singleRun: true,

    // Concurrency level
    // how many browser should be started simultaneous
    concurrency: Infinity
  })
}
