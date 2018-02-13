// Karma configuration
// Generated on Mon Jan 29 2018 11:58:53 GMT+0700 (SE Asia Standard Time)

// Super-useful!  http://www.bradoncode.com/blog/2015/02/27/karma-tutorial/#handling-html-fixtures

module.exports = {
  // base path that will be used to resolve all patterns (eg. files, exclude)
  basePath: '..',

  // frameworks to use
  // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
  frameworks: ['mocha', 'chai', 'fixture'],

  // list of files / patterns to load in the browser
  files: [
    'unit_tests/test_utils.js', // A basic utility script useful for constructing tests
    'unit_tests/modernizr.js', // A dependency-managed utility script that helps with browser feature detection.
    'unit_tests/recorder_InputEvents.js', // The object definitions used to generate/replicate key events for engine tests.
    'unit_tests/recorder_InputEvents.map', // The object definitions used to generate/replicate key events for engine tests.
    'unit_tests/cases/**/*.js', // Where the tests actually reside.
    'unit_tests/json/**/*.json', // Where pre-loaded JSON resides.
    {pattern: 'unit_tests/resources/**/*.*', watched: true, served: true, included: false}, // General testing resources.
    {pattern: 'release/unminified/web/**/*.css', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'release/unminified/web/**/*.gif', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'release/unminified/web/**/*.png', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'release/unminified/web/**/*.eot', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'release/unminified/web/**/*.ttf', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'release/unminified/web/**/*.woff', watched: false, served: true, included: false}, // OSK resources
    {pattern: 'release/unminified/web/*.js', watched: true, served: true, included: false},  // The actual KMW code.
    {pattern: 'release/unminified/web/*.map', watched: true, served: true, included: false}, // + sourcemaps.
    {pattern: 'unit_tests/fixtures/**/*.html', watched: true} // HTML structures useful for testing.
  ],

  proxies: {
    "/source/": "/base/release/unminified/web/",
    "/resources/": "/base/unit_tests/resources/"
  },


  // list of files / patterns to exclude
  exclude: [
  ],

  // preprocess matching files before serving them to the browser
  // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
  preprocessors: {
    'unit_tests/fixtures/**/*.html'	: ['html2js'],
    'unit_tests/json/**/*.json' : ['json_fixtures']
  },

  // Settings to properly configure how JSON fixtures are automatically loaded by Karma.
  jsonFixturesPreprocessor: {
    stripPrefix: 'unit_tests/json',
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
