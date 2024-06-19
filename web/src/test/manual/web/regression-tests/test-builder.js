// Generate a tests-generated for all available keyboards
// We do this because dynamically setting up test cases using Mocha + karma is
// fragile at best -- and does not report the total number of cases correctly.

const fs = require('fs');
const path = require('path');
const config = require('./node_src/config.js');
const util = require('./node_src/util.js');
const program = require('commander');

const KEYBOARDS_ROOT = path.join(config.KEYBOARDS_ROOT, config.KEYBOARDS_GROUP);

function list(val) {
  return val.split(',');
}

program
  .version('0.1')
  .option('-c, --compiler-version [version]', 'Specify compiler version that is being tested, which is saved in the results file.')
  .option('-e, --engine-version [version]', 'Specify KeymanWeb engine version that is being tested, which is saved in the results file.')
  .option('-k, --keyboards [keyboards]', 'Test specific keyboard(s) in the repo, e.g. k/khmer_angkor. If -k is not specified, then test all keyboards in the keyboards repository.', list, []);

program.parse(process.argv);

config.compilerVersion = program.compilerVersion;
config.engineVersion = program.engineVersion;

let keyboards = util.getKeyboardFolders(KEYBOARDS_ROOT, true);

let code = `
  import { assert } from '../../../../../../node_modules/chai/chai.js';
`;

keyboards.forEach(function(keyboard) {
  if(program.keyboards.length == 0 || program.keyboards.indexOf(keyboard.shortname+'/'+keyboard.id) >= 0) {
    code += `
      describe('Test keyboard ${keyboard.id}', () => {
        const shortname='${keyboard.shortname}';
        const id='${keyboard.id}';
        const locator = shortname+'/'+id;
        it('should generate a set of results for "all" possible inputs', function() {
          return windowLoad
            .then(() => testRunner.loadTests(locator))
            .then((shouldRun) => { if(shouldRun) { return testRunner.runTests(id); } })
            .then(() => testRunner.saveTestResults(locator, (testRunner.keyboards[id] || {}).results));
        }).timeout(0);
      });
    `;
  }
});

fs.writeFileSync('./tests-generated.js', code);

//
// Run Karma with the generated tests
//

const Server = require('karma').Server;
const cfg = require('karma').config;

// Note: if karma.conf.js is invalid, then this will die with exit code 1
// without logging the error. The easiest way to see the error is to
// run `node_modules/bin/karma start karma.conf.js`
const karmaConfig = cfg.parseConfig(path.resolve('./karma.conf.js'), process.env.TEAMCITY_PROJECT_NAME ? {'reporters': ['teamcity']} : {});

let server = new Server(karmaConfig, function(exitCode) {
  console.log('Karma has exited with ' + exitCode)
  process.exit(exitCode)
});

server.start();
