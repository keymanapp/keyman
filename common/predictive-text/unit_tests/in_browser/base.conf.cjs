// Karma configuration

// Super-useful!  http://www.bradoncode.com/blog/2015/02/27/karma-tutorial/#handling-html-fixtures

module.exports = {
  // base path that will be used to resolve all patterns (eg. files, exclude)
  basePath: '../../../../', // Root directory of the repository(!)

  client: {
    /* `client.args` here is passed to the test runner page as `__karma__.config.args`.
     *
     * Karma doc type spec says "array", so we use an array.  It also gives us room to add alternate
     * configuration details later if we need to, though on a CI vs local basis only.
     *
     * Timeouts below are in milliseconds
     */
    args: [{
      type: "timeouts", // This base is designed for local machine testing.  These provide configurable timeout settings
                        // accessible as in-browser variables to unit tests.
      eventDelay: 50,   // Designed for small delays to allow time for event handling to occur before proceeding.
                        // Make sure this stays under 1/4 of 'standard', as multiple eventDelays may occur within a test.
      standard: 4000,
      scriptLoad: 6000,
      mobileFactor: 1   // An extra timeout modifier to be applied when running on a mobile device.
    }]
  },

  // frameworks to use
  // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
  frameworks: ['mocha', 'fixture'],

  // list of files / patterns to load in the browser
  files: [
    { pattern: 'node_modules/chai/chai.js', served: true, watched: false, included: false, type: 'module'},
    // { pattern: 'node_modules/**/*.js', served: true, watched: false, included: false },
    // { pattern: 'node_modules/**/*.mjs', served: true, watched: false, included: false, type: "module" },
    // { pattern: 'node_modules/**/*.cjs', served: true, watched: false, included: false },

    // Provides utility helpers and objects for tests.
    { pattern: 'common/predictive-text/unit_tests/in_browser/helpers.mjs', served: true, watched: true, included: false},

    // Where the tests actually reside.
    { pattern: 'common/predictive-text/unit_tests/in_browser/cases/**/*.js', type: 'module' },

    'common/test/resources/*.js',
    'common/test/resources/json/models/**/*.json',
    { pattern: 'common/web/lm-worker/build/lib/*.js', watched: true, served: true, included: false},
    { pattern: 'common/web/lm-worker/build/lib/*.js.map', watched: true, served: true, included: false},
    { pattern: 'common/predictive-text/build/obj/**/*.*', watched: true, served: true, included: false },
    { pattern: 'common/predictive-text/build/obj/**/*.js.map', watched: true, served: true, included: false },
    { pattern: 'common/predictive-text/build/lib/**/*.*', watched: true, served: true, included: false },
    { pattern: 'common/predictive-text/build/lib/**/*.js.map', watched: true, served: true, included: false },

    // We don't have anything in these locations... yet.  But they'll be useful for test resources.
    {pattern: 'common/test/resources/**/*.*', watched: true, served: true, included: false}, // General testing resources.
  ],

  // list of files / patterns to exclude
  exclude: [
  ],

  // preprocess matching files before serving them to the browser
  // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
  preprocessors: {
    'common/test/resources/json/models/**/*.json' : ['json_fixtures']
  },

  // Settings to properly configure how JSON fixtures are automatically loaded by Karma.
  jsonFixturesPreprocessor: {
    stripPrefix: 'common/test/resources/json/',
    variableName: '__json__'
  },

  proxies: {
    "/resources/": "/base/common/test/resources/",
    // "/node_modules/": "/base/node_modules/",
    // "/@keymanapp/lm-worker/": "/base/node_modules/@keymanapp/lm-worker/"
  },

  // web server port
  port: 9876,

  // enable / disable colors in the output (reporters and logs)
  colors: true,

  // TEMP ENTRY!
  reporters: ['teamcity'],

  // enable / disable watching file and executing tests whenever any file changes
  autoWatch: true,

  // Continuous Integration mode
  // if true, Karma captures browsers, runs the tests and exits
  // if false, it generates the pages and acts as a persistent server.
  singleRun: true,
}
