
/**
 * config: Configurable parameters for regression tests
 */

/*
  For now, with Karma's file serving configuration, we're assuming the
  keyboards repository is in a folder at the same level as this repo's
  root. Add "release/" because we only work with released keyboards.

  The keyboards repository can be cloned from https://github.com/keymanapp/keyboards
*/

const path = require('path');

module.exports = {
  host: "127.0.0.1",
  port: 1337,
  /* ######################################
     You should avoid changing values below
     ######################################
   */
  /**
   * The path to the keyboards repository, relative to the current folder.
   * Because of how Karma serves files, for now you should avoid changing
   * this.
   */
  KEYBOARDS_ROOT: path.join(path.resolve(__dirname, '..'), '..', '..', '..', '..', 'keyboards'),
  /**
   * The section of the keyboards repo we will test (you could change this
   * to `experimental` if you have your own experimental keyboards to test)
   */
  KEYBOARDS_GROUP: 'release',
  /**
   * The path to the built version of KeymanWeb that we are testing.
   */
  KEYMANWEB_ROOT: path.join(path.resolve(__dirname, '..'), '..', '..', 'release', 'unminified', 'web'),
  /**
   * Various relative paths
   */
  KEYMANWEB_RELATIVE_PATH: 'web/build/app/web/debug',
  BASE_RELATIVE_PATH: '../../../../',
  KEYMAN_REPO_BASE_RELATIVE_PATH: '../../../',

  /**
   * These variables are usually set at runtime per the CI environment, and
   * if not set then will be inferred from the compiler and engine.
   */
  engineVersion: null,
  compilerVersion: null,
};
