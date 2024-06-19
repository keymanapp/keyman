// Karma configuration
// Generated on Wed Mar 13 2019 10:21:54 GMT+1100 (Australian Eastern Daylight Time)
const path = require('path');
const test_config = require('./node_src/config.js');
const test_host = require('./node_src/test-host.js');

module.exports = function(config) {
  //
  // This assumes that the keyboards repo is at the same level as the KeymanWeb repo
  // and has the folder name 'keyboards'. If this script is moved from its current
  // location, these paths will need adjustment.
  //

  // Fortunately, it is easy to ensure that with our CI setup using TeamCity, that
  // the keyboards repo has a consistent location relative to the keyman repo, so
  // for now we should be able to live with this setup.

  // The name of the keyboards repo base folder, relative to base (same level as this
  // keyman repo, by default).
  const KEYBOARDS_REPO_BASENAME = path.basename(test_config.KEYBOARDS_ROOT);

  // The location of the keymanweb.js build files, relative to the root of the keyman
  // repo.
  const KEYMANWEB_RELATIVE_PATH = test_config.KEYMANWEB_RELATIVE_PATH;

  // The following constants should not need modification, unless we move files around
  const base = path.resolve(__dirname, test_config.BASE_RELATIVE_PATH);
  const keyboards_root = path.join(base, KEYBOARDS_REPO_BASENAME);
  const keyman_root = path.resolve(__dirname, test_config.KEYMAN_REPO_BASE_RELATIVE_PATH);
  const keyman_basename = path.basename(keyman_root);
  const test_relative_path = __dirname.substr(keyman_root.length + 1); // excludes initial path delimiter

  /*
  console.log('__dirname                = ' + __dirname);
  console.log('base                     = ' + base);
  console.log('keyboards_root           = ' + keyboards_root);
  console.log('keyman_root              = ' + keyman_root);
  console.log('keyman_basename          = ' + keyman_basename);
  console.log('test_relative_path       = ' + test_relative_path);
  console.log('KEYBOARDS_REPO_BASENAME  = ' + KEYBOARDS_REPO_BASENAME);
  console.log('KEYMANWEB_RELATIVE_PATH  = ' + KEYMANWEB_RELATIVE_PATH);
  */

  function KeymanwebRegressionFooFactory(config) {
    return function (request, response, /* next */) {
      debugger;
      response.writeHead(200);
      return response.end('FOO!!');
    };
  }

  config.set({

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: base,

    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['mocha', 'chai'],

    // list of files / patterns to load in the browser
    files: [
      // Test framework
      { pattern: path.join(keyman_basename, test_relative_path, 'test.html'), nocache: true },
      
      // KeymanWeb should come before the tests are loaded - test-runner.js in particular
      // requires the `keyman` object.  At this point, we only include the main script itself.
      { pattern: path.join(keyman_basename, test_relative_path, 'web', 'unminified', 'keymanweb.js'), watched: false, included: true, nocache: true },
      
      { pattern: path.join(keyman_basename, test_relative_path, 'tests-generated.js'), nocache: true },

      // The test-runner is shared with the Node application for generating base tests manually
      { pattern: path.join(keyman_basename, test_relative_path, 'src', 'test-runner.js'), nocache: true }, 
      { pattern: path.join(keyman_basename, test_relative_path, 'src', 'known-failures.js'), nocache: true }, 

      // KeymanWeb resources, source, and Keyboards
      //{ pattern: path.join(keyman_basename, KEYMANWEB_RELATIVE_PATH, '**'), watched: false, included: false, nocache: true },
      { pattern: path.join(keyman_basename, test_relative_path, 'web', 'unminified', '**'), watched: false, included: false, nocache: true },
      { pattern: path.join(KEYBOARDS_REPO_BASENAME, 'release', '**'), watched: false, included: false, nocache: true },
    ],

    // Add our custom HTTP responders for `/save-results` and `/list-keyboards`

    middleware: ['custom'],

    plugins: [
      'karma-*',
       {'middleware:custom': ['factory', function(/*config*/) { return test_host.handleRequest; }]}
    ],

    // proxy the convoluted relative paths into consistent targets at /web/ and /keyboards/

    proxies: {
      '/web/' : { target: '/base/' + path.posix.join(keyman_basename, test_relative_path, 'web', 'unminified') + '/' },
      //'/web/' : { target: '/base/' + path.posix.join(keyman_basename, KEYMANWEB_RELATIVE_PATH) + '/' },
      '/keyboards/' : { target: '/base/' + path.posix.join(KEYBOARDS_REPO_BASENAME, test_config.KEYBOARDS_GROUP) + '/' },
    },

    // list of files / patterns to exclude
    exclude: [
    ],


    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
    },


    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['progress'],


    // web server port
    port: 9876,


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,


    // enable / disable watching file and executing tests whenever any file changes
    //TODO enable watch for interactive tests?
    autoWatch: false,


    // Some of our larger tests take over a minute to complete
    browserNoActivityTimeout: 240000,


    // start these browsers
    // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
    browsers: ['Chrome'],


    // Continuous Integration mode
    // if true, Karma captures browsers, runs the tests and exits
    //TODO disable singleRun for interactive tests? - split into separate conf file?
    singleRun: true,

    // Concurrency level
    // how many browser should be started simultaneous
    concurrency: Infinity,

  })
}
