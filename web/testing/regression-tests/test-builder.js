// Generate a test-runner for all available keyboards
// We do this because dynamically setting up test cases using Mocha + karma is
// fragile at best -- and does not report the total number of cases correctly.

const fs = require('fs');
const path = require('path');
const config = require('./config.js');
const util = require('./util.js');

const KEYBOARDS_ROOT = path.join(config.KEYBOARDS_ROOT, config.KEYBOARDS_GROUP);

let keyboards = util.getKeyboardFolders(KEYBOARDS_ROOT);

let code = `
  let assert = chai.assert;
`;

console.log(process.argv[2]);
keyboards.forEach(function(keyboard) {
 
  if(process.argv.length < 2 || keyboard.id == process.argv[2])

  code += `

  describe('Test keyboard ${keyboard.id}', () => {
    const s='${keyboard.s}';
    const id='${keyboard.id}';
    const locator = s+'/'+id;
    it('should generate a set of results for "all" possible inputs', function() {
      return windowLoad
        .then(function() {
          return testRunner.loadTests(locator)
            .then(() => testRunner.runTests(id))
            .then(() => testRunner.saveTestResults(locator, testRunner.keyboards[id].results));
          });
    }).timeout(0);
  });
  
`;
});

fs.writeFileSync('./tests-generated.js', code);

//
// Run Karma with the generated tests
//

const Server = require('karma').Server
const cfg = require('karma').config;

const karmaConfig = cfg.parseConfig(path.resolve('./karma.conf.js'), { port: 1337 } );

let server = new Server(karmaConfig, function(exitCode) {
  console.log('Karma has exited with ' + exitCode)
  process.exit(exitCode)
});

server.start();