// Karma configuration
// Generated on Mon Jan 29 2018 11:58:53 GMT+0700 (SE Asia Standard Time)

// Super-useful!  http://www.bradoncode.com/blog/2015/02/27/karma-tutorial/#handling-html-fixtures

module.exports = {
  // base path that will be used to resolve all patterns (eg. files, exclude)
  // set to Keyman repo base.
  basePath: '../..',

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
      mobileFactor: 1 // An extra timeout modifier to be applied when running on a mobile device.
    }]
  },

  // frameworks to use
  // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
  frameworks: ['mocha', 'chai', 'fixture'],

  // list of files / patterns to load in the browser
  files: [
    'web/unit_tests/modernizr.js', // A dependency-managed utility script that helps with browser feature detection.
    'web/unit_tests/recorder_InputEvents.js', // The object definitions used to generate/replicate key events for engine tests.
                                          // Includes KMW's Device class, which is used by test_utils below.
    'web/unit_tests/dev_resources.js',  // Defines com.keyman.dom objects separate from KMW for unit testing.
    'web/unit_tests/test_init_check.js', // Ensures that tests will initialize properly
    'web/unit_tests/test_utils.js', // A basic utility script useful for constructing tests
    'web/unit_tests/cases/**/*.js', // Where the tests actually reside.
    'common/core/web/tests/resources/json/**/*.json', // Where pre-loaded JSON resides.
    {pattern: 'common/core/web/tests/resources/fixtures/**/*.html', watched: true}, // HTML structures useful for testing.
    {pattern: 'common/core/web/tests/resources/**/*.*', watched: true, served: true, included: false}, // General testing resources.
    {pattern: 'web/release/unminified/web/**/*.css', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/release/unminified/web/**/*.gif', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/release/unminified/web/**/*.png', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/release/unminified/web/**/*.eot', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/release/unminified/web/**/*.ttf', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/release/unminified/web/**/*.woff', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/release/unminified/web/*.js', watched: true, served: true, included: false},  // The actual KMW code.
    {pattern: 'web/release/unminified/web/*.map', watched: true, served: true, included: false}, // + sourcemaps.
    {pattern: 'web/unit_tests/recorder_InputEvents.js.map', watched: true, served: true, included: false},
    {pattern: 'web/unit_tests/element-interface.js.map', watched: true, served: true, included: false},
    {pattern: 'web/unit_tests/dev_resources.js.map', watched: true, served: true, included: false}
  ],

  proxies: {
    "/source/": "/base/web/release/unminified/web/",
    "/resources/": "/base/common/core/web/tests/resources/",
    "/source/recorder_InputEvents.js.map": "/base/common/core/web/tests/recorder_InputEvents.js.map"
  },


  // list of files / patterns to exclude
  exclude: [
  ],

  // preprocess matching files before serving them to the browser
  // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
  preprocessors: {
    'common/core/web/tests/resources/fixtures/**/*.html'	: ['html2js'],
    'common/core/web/tests/resources/json/**/*.json' : ['json_fixtures']
  },

  html2JsPreprocessor: {
    stripPrefix: 'common/core/web/tests/resources/'
  },

  // Settings to properly configure how JSON fixtures are automatically loaded by Karma.
  jsonFixturesPreprocessor: {
    stripPrefix: 'common/core/web/tests/resources/json',
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
