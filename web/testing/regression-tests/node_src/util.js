const fs = require('fs');
const path = require('path');

module.exports = {
  /**
   * Get an array of keyboard layouts available
   * @param {string} root
   * @param {Boolean?} onlyKeyboardsWithTests 
   * @returns {Array}
   */
  getKeyboardFolders: function(root, onlyKeyboardsWithTests) {
    let shortnames = fs.readdirSync(root);
    let keyboards = [];
    shortnames.forEach(function(shortname) {
      let kbds = fs.readdirSync(path.join(root, shortname));
      if(kbds && kbds.length) {
        kbds.forEach(function(kbd) {
          if((!onlyKeyboardsWithTests || fs.existsSync(path.join(root, shortname, kbd, 'tests', kbd+'.tests'))) &&
              fs.existsSync(path.join(root, shortname, kbd, 'build', kbd+'.js'))) {
            keyboards.push({shortname: shortname, id: kbd});
          }
        });
      }
    });
    return keyboards;
  },

  /**
   * Validates and parses a locator string (shortname/id, e.g. k/kayan) into an object.
   * Note: currently duplicated in test-runner.js for web version (could unify one day...)
   * @param {string} locator   "shortname/id", e.g. "k/kayan"
   * @returns {object}  object with `shortname` and `id` properties
   */
  parseLocator: function(locator) {
    let m = locator.match(/^([a-z0-9_]+)\/([a-z0-9_]+)$/);
    return m ?
      {shortname: m[1], id: m[2]} :
      {shortname: null, id: null};
  },

  runProcess: function(command, params, options, ignoreExitCode) {
    return new Promise((resolve, reject) => {
      let spawn = require('child_process').spawn;
      let build = spawn(command, params, options);

      // We pipe the stdout/stderr rather than using stdio:'inherit' option because
      // stdio:'inherit' changes the mode of the console streams and we lose colours
      // etc.
      build.stdout.on('data', function(data) {
        fs.writeSync(1, data.toString());
      });
      build.stderr.on('data', function(data) {
        fs.writeSync(2, data.toString());
      });

      build.on('exit', function(code) {
        if(code != 0) {
          if(ignoreExitCode) {
            console.warn(`Process ${command} finished with code ${code}`);
          } 
          else {
            reject(`Process ${command} failed with code ${code}`);
          }
        }
        resolve();
      });
    });
  }
}
