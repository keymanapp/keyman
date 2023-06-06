// Karma configuration
// Generated on Mon Jan 29 2018 11:58:53 GMT+0700 (SE Asia Standard Time)

// Super-useful!  http://www.bradoncode.com/blog/2015/02/27/karma-tutorial/#handling-html-fixtures

module.exports = {
  // base path that will be used to resolve all patterns (eg. files, exclude),
  // set to this module's base folder.
  basePath: '../../../..',

  // Safeguards against disconnecting after starting tests.
  browserNoActivityTimeout: 60000,

  client: {
    /* `client.args` here is passed to the test runner page as `__karma__.config.args`.
     *
     * Karma doc type spec says "array", so we use an array.  It also gives us room to add alternate
     * configuration details later if we need to, though on a CI vs local basis only.
     *
     * Timeouts below are in milliseconds
     */
    args: [{
      type: "timeouts", // This base is designed for local machine testing.
      eventDelay: 60, // Designed for small delays to allow time for event handling to occur before proceeding.
                      // Make sure this stays under 1/4 of 'standard', as multiple eventDelays may occur within a test.
      standard: 5000,
      scriptLoad: 8000,
      uiLoad: 30000, // Loads two scripts + includes internal setup/timeout time requirements.
                     // At this time of writing this, UI script loading is one of the longest checks.
    }]
  },

  // frameworks to use
  // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
  frameworks: ['mocha', 'chai', 'fixture'],

  // list of files / patterns to load in the browser
  files: [
    { pattern: 'build/lib/index.mjs', type: 'module' }, // The primary gesture-recognizer script.
    { pattern: 'build/lib/index.mjs.map', watched: true, served: true, included: false },

    { pattern: 'build/tools/lib/index.mjs', type: 'module' }, // The primary unit-test-resources script.
    { pattern: 'build/tools/lib/index.js.map', watched: true, served: true, included: false },

    '../../../node_modules/sinon/pkg/sinon.js',

    '../../../common/test/resources/timeout-adapter.js', // Handles timeout configuration for local vs BrowserStack-based testing

    { pattern: 'src/test/auto/browser/cases/**/*.js', type: 'module' }, // Where the tests actually reside.

    'build/tools/host-fixture.html', // The primary test fixture's build output location
    'build/tools/gestureHost.css', // The primary test fixture's backing CSS stylesheet.  Note:  Karma will auto-link it!
    'src/test/resources/json/**/*.json', // Where pre-loaded JSON resides.
  ],

  proxies: {
    "/resources/": "/src/test/resources/",
  },


  // list of files / patterns to exclude
  exclude: [
  ],

  // preprocess matching files before serving them to the browser
  // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
  preprocessors: {
    'build/tools/host-fixture.html': ['html2js'],
    'src/test/resources/fixtures/**/*.html'	: ['html2js'],
    'src/test/resources/json/**/*.json' : ['json_fixtures']
  },

  html2JsPreprocessor: {
    stripPrefix: 'build/tools'
  },

  // Settings to properly configure how JSON fixtures are automatically loaded by Karma.
  jsonFixturesPreprocessor: {
    stripPrefix: 'src/test/resources/json/',
    variableName: '__json__'
  },

  // web server port
  port: 9876,

  // enable / disable colors in the output (reporters and logs)
  colors: true,

  // enable / disable watching file and executing tests whenever any file changes
  autoWatch: true,

  // Continuous Integration mode
  // if true, Karma captures browsers, runs the tests and exits
  // if false, it generates the pages and acts as a persistent server.
  singleRun: true,
}
