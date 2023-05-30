module.exports = function(config) {
  var base = require("./base.conf.cjs");

  var specifics = {
    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['mocha'],

    // start these browsers
    // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
    browsers: ['Chrome'], // Can be specified at run-time instead!
	  // Future note for us:  https://www.npmjs.com/package/karma-browserstack-launcher

    // Concurrency level
    // how many browser should be started simultaneous
    concurrency: Infinity,

    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    // Can't set in base.conf.cjs b/c of constant's definition style.
    logLevel: config.LOG_INFO
  };

  config.set(Object.assign(specifics, base));
}
