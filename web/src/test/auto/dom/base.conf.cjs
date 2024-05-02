// Karma configuration
// Generated on Mon Jan 29 2018 11:58:53 GMT+0700 (SE Asia Standard Time)

// Super-useful!  http://www.bradoncode.com/blog/2015/02/27/karma-tutorial/#handling-html-fixtures

module.exports = {
  // base path that will be used to resolve all patterns (eg. files, exclude)
  // set to Keyman repo base.
  basePath: '../../../../..',

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
  frameworks: ['mocha', 'fixture'],

  // list of files / patterns to load in the browser
  files: [
    { pattern: 'node_modules/chai/chai.js', watched: true, served: true, included: false, type: 'module'},
    {pattern: 'web/src/test/auto/dom/kbdLoader.mjs', type: 'module', watched: true, served: true, included: false},

    {pattern: 'web/src/test/auto/dom/test_init_check.js', type: 'module'},         // Ensures that tests will initialize properly
    'common/test/resources/timeout-adapter.js',  // Handles configuration timeout setup at runtime.
    {pattern: 'web/src/test/auto/dom/test_utils.js', type: 'module'},              // A basic utility script useful for constructing tests

    {pattern: 'web/src/test/auto/dom/cases/**/*.def.js', watched: false, served: false, included: false},
    {pattern: 'web/src/test/auto/dom/cases/**/*.js', type: 'module'},              // Where the tests actually reside.
    'common/test/resources/json/**/*.json',      // Where pre-loaded JSON resides.
    {pattern: 'web/src/test/auto/resources/**/*.js.fixture', watched: true, served: true, included: false},

    {pattern: 'web/build/**/*.js',      watched: true, served: true, included: false}, // Includes all top-level KMW products
    {pattern: 'web/build/**/*.js.map',  watched: true, served: true, included: false}, // and their sourcemaps.
    {pattern: 'web/build/**/*.mjs',     watched: true, served: true, included: false}, // Includes all top-level KMW products
    {pattern: 'web/build/**/*.mjs.map', watched: true, served: true, included: false}, // and their sourcemaps.
    { pattern: 'common/predictive-text/build/obj/**/*.*', watched: true, served: true, included: false },
    { pattern: 'common/predictive-text/build/obj/**/*.js.map', watched: true, served: true, included: false },

    {pattern: 'common/web/**/*.mjs', watched: true, served: true, included: false},
    {pattern: 'common/web/**/*.mjs.map', watched: true, served: true, included: false},

    {pattern: 'common/test/resources/fixtures/**/*.html', watched: true}, // HTML structures useful for testing.
    {pattern: 'common/test/resources/**/*.*', watched: true, served: true, included: false}, // General testing resources.
    {pattern: 'web/src/test/auto/resources/fixtures/**/*.html', watched: true, served: true}, // Fixtures for use in engine/attachment tests
    {pattern: 'web/src/test/auto/resources/html/**/*.html', watched: true, served: true}, // Generally, pages for hosting in iframe fixtures.

    {pattern: 'web/build/app/web/debug/**/*.css', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/build/app/web/debug/**/*.gif', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/build/app/web/debug/**/*.png', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/build/app/web/debug/**/*.ttf', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/src/resources/**/*.ttf', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/src/resources/**/*.css', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/src/resources/**/*.gif', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/src/resources/**/*.png', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'web/build/app/web/debug/*.js', watched: true, served: true, included: false},  // The actual KMW code.
    {pattern: 'web/build/app/web/debug/*.map', watched: true, served: true, included: false}, // + sourcemaps.
    {pattern: 'web/build/app/ui/debug/*.js', watched: true, served: true, included: false},  // The actual KMW UI code.
    {pattern: 'web/build/app/ui/debug/*.map', watched: true, served: true, included: false}, // + sourcemaps.
    {pattern: 'web/build/tools/testing/recorder/obj/index.js.map', watched: true, served: true, included: false},
    {pattern: 'web/build/engine/element-wrappers/obj/index.bundled.js.map', watched: true, served: true, included: false},
    {pattern: 'web/build/engine/device-detect/obj/index.bundled.js.map', watched: true, served: true, included: false},
    {pattern: 'node_modules/sinon/**/*.*', watched: false, served: true, included: false}
  ],

  proxies: {
    "/source/": "/base/web/build/app/web/debug/",
    "/ui-source/": "/base/web/build/app/ui/debug/",
    "/resources/": "/base/common/test/resources/",
    "/source/recorder_InputEvents.js.map": "/base/common/tests/recorder_InputEvents.js.map",
    "/node_modules/": "/base/node_modules/",
    "/@keymanapp/web-utils/": "/base/common/web/utils/",
    "/@keymanapp/keyman/": "/base/web/",
    "/@keymanapp/keyboard-processor/": "/base/common/web/keyboard-processor/",
    "/html/": "/base/web/src/test/auto/resources/html/"
  },


  // list of files / patterns to exclude
  exclude: [
  ],

  // preprocess matching files before serving them to the browser
  // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
  preprocessors: {
    'web/src/test/auto/resources/fixtures/**/*.html' : ['html2js'],
    'common/test/resources/json/**/*.json' : ['json_fixtures']
  },

  html2JsPreprocessor: {
    stripPrefix: 'web/src/test/auto/resources/'
  },

  // Settings to properly configure how JSON fixtures are automatically loaded by Karma.
  jsonFixturesPreprocessor: {
    stripPrefix: 'common/test/resources/json',
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
