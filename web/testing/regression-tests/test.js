// test.js

// This is the primary framework for running regression tests

const fs = require('fs-extra');
const path = require('path');
const got = require('got');
const AdmZip = require('adm-zip');
const program = require('commander');
const rimraf = require('rimraf');

// Local modules

const config = require('./config.js');
const util = require('./util.js');

function list(val) {
  return val.split(',');
}

program
  .version('0.1')
  .option('-c, --compiler-versions [versions]', 'Specify compiler version(s) to test. Can specify "stable", "source" or a specific version number.', list, ['stable','source'])
  .option('-e, --engine-versions [versions]', 'Specify KeymanWeb engine version(s) to test. Can specify "stable", "source" or a specific version number.', list, ['stable','source'])
  .option('-k, --keyboards [keyboards]', 'Builds and tests specific keyboard source files. If -k is not specified, then test all keyboards in the keyboards repository.', list, [])
  .option('-f, --fail-fast', "Don't attempt to continue tests after the first keyboard test fails")
  .option('--skip-analysis', "Don't create .tests files, assume they are already present");

program.parse(process.argv);

const KEYBOARDS_ROOT = path.join(config.KEYBOARDS_ROOT, config.KEYBOARDS_GROUP);

process.exitCode = 0;

//console.log(program.compilerVersions);
//console.log(program.engineVersions);

// Environment

// TODO read env var GIT_BASH_FOR_KEYMAN? "C:\Program Files\Git\bin\bash.exe" --init-file "c:\Program Files\Git\etc\profile" -l
const bash = process.platform == 'win32'
  ? { command: 'C:\\Program Files\\Git\\bin\\bash.exe', params: ['--init-file', 'C:\\Program Files\\Git\\etc\\profile', '-l'] }
  : { command: 'bash', params: [] };

//TODO: support analysis
//TODO: support specifying standalone keyboards

const compilerDestPath = path.join(config.KEYBOARDS_ROOT, 'tools');
const kmcompDestPath = path.join(compilerDestPath, 'kmcomp.exe');
const kmcmpdllDestPath = path.join(compilerDestPath, 'kmcmpdll.dll');

let testedCompilerVersions = [], testedEngineVersions = [];

// Build keyboards -- get kmcomp.exe, kmcmpdll.dll from appropriate location

fs.renameSync(kmcompDestPath, kmcompDestPath + '.bak');
fs.renameSync(kmcmpdllDestPath, kmcmpdllDestPath + '.bak');

process.on('exit', () => {
  console.log('Restoring original compiler files');
  fs.renameSync(kmcompDestPath + '.bak', kmcompDestPath);
  fs.renameSync(kmcmpdllDestPath + '.bak', kmcmpdllDestPath);
});

process.on('SIGINT', () => {
  console.log('Received Ctrl+C, aborting');
  process.exit(2);
});

process.on('unhandledRejection', (reason, p) => {
  console.log('Unhandled Rejection at:', p, 'reason:', reason);
  process.exit(9);
});

/**
 * 
 * @param items An array of items.
 * @param fn A function that accepts an item from the array and returns a promise.
 * @returns {Promise}
 */
function forEachPromise(items, fn) {
  return items.reduce(function (promise, item) {
      return promise.then(function () {
          return fn(item);
      });
  }, Promise.resolve());
}

function fail(msg, code) {
  // We don't abandon the test cases unless fail-fast is specified but we do have a finish failure
  console.error(msg);
  process.exitCode = code;
  if(program.failFast) process.exit();
}

forEachPromise(program.compilerVersions, version => {
  // 
  // Compile phase - get the compiler
  //

  console.log('Retrieving compiler, version '+version);
  let getCompiler, compilerVersion;
  switch(version) {
    case 'source':
      getCompiler = new Promise((resolve, reject) => {
        compilerVersion = 'source';
        if(process.platform !== 'win32') {
          console.error('Source build of compiler is only available on Windows.');
          reject();
        }
        fs.copyFileSync(path.join(config.KEYMAN_REPO_BASE_RELATIVE_PATH, 'windows', 'bin', 'developer', 'kmcomp.exe'), kmcompDestPath);
        fs.copyFileSync(path.join(config.KEYMAN_REPO_BASE_RELATIVE_PATH, 'windows', 'bin', 'developer', 'kmcmpdll.dll'), kmcmpdllDestPath);
        resolve();
      });
      break;
    case 'stable':
      getCompiler = got('https://downloads.keyman.com/api/version/developer/2.0', { json: true })
        .then(response => {
          compilerVersion = response.body.developer.stable.version;
          console.log('Downloading compiler version '+compilerVersion);
          return got(`https://downloads.keyman.com/developer/stable/${compilerVersion}/kmcomp-${compilerVersion}.zip`, { encoding: null });
        })
        .then(response => {
          console.log('Unzipping compiler');
          let zip = new AdmZip(response.body);
          zip.extractEntryTo('kmcomp.exe', compilerDestPath, false, true);
          zip.extractEntryTo('kmcmpdll.dll', compilerDestPath, false, true);
        });
      break;
    default:
      console.log('Downloading specified compiler version '+version);
      compilerVersion = version;
      getCompiler = got(`https://downloads.keyman.com/developer/stable/${version}/kmcomp-${version}.zip`, { encoding: null })
        .then(response => {
          console.log('Unzipping compiler');
          let zip = new AdmZip(response.body);
          zip.extractEntryTo('kmcomp.exe', compilerDestPath, false, true);
          zip.extractEntryTo('kmcmpdll.dll', compilerDestPath, false, true);
        });
  }

  //
  // Compile phase -- build the keyboards
  //

  let runCompile = getCompiler.then(() => {
    // Shell out to build script
    // TODO: Support building only js targets (requires update to keyboards build scripts)
    // TODO: Support debug builds (-d)
    console.log(`Compiling keyboards with version ${compilerVersion} [${version}]`);

    testedCompilerVersions.push(compilerVersion);

    let buildKeyboard = function(keyboard) {
      keyboard = keyboard ? 'release/'+keyboard : 'release';
      return new Promise((resolve, reject) => {
        let spawn = require('child_process').spawn;
        let build = spawn(`${bash.command}`, [].concat(bash.params, ['build.sh', '-T', 'kmn', keyboard]), { cwd: config.KEYBOARDS_ROOT });

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
            reject('Keyboards build failed with code '+code);
          }
          resolve();
        });
      });
    };
  
    return (program.keyboards.length) ? forEachPromise(program.keyboards, buildKeyboard) : buildKeyboard('');
  });

  //
  // TODO: Analyze phase (for first compile run only)
  // TODO: use the js-target kmx intermediate
  //

  //
  // Test phase, multiple tests to run
  //

  let testActions = runCompile.then(() => {
    console.log('========= Starting tests =========');
    return forEachPromise(program.engineVersions, (version) => {

      let getEngine, engineVersion;
      switch(version) {
        case 'source':
          getEngine = new Promise((resolve, reject) => {
            engineVersion = 'source';
            // TODO: Assuming for now that the intermediate version is already built.
            if(fs.existsSync('web/')) {
              rimraf.sync('web/');
            }
            fs.mkdirSync('web');
            fs.mkdirSync('web/unminified/');
            fs.copySync(config.KEYMANWEB_ROOT + '/', 'web/unminified/');
            resolve();
          });
          break;
        case 'stable':
          getEngine = got('https://downloads.keyman.com/api/version/web/2.0', { json: true })
            .then(response => {
              engineVersion = response.body.web.stable.version;
              console.log('Downloading engine version '+engineVersion);
              return got(`https://downloads.keyman.com/web/stable/${engineVersion}/keymanweb-${engineVersion}.zip`, { encoding: null });
            })
            .then(response => {
              console.log('Unzipping engine');
              // Assuming engine files we want are at unminified/ in the zip
              let zip = new AdmZip(response.body);
              zip.extractEntryTo('unminified/', 'web/', true, true);
            });
          break;
        default:
          console.log('Downloading specified engine version '+version);
          engineVersion = version;
          getEngine = got(`https://downloads.keyman.com/web/stable/${version}/keymanweb-${version}.zip`, { encoding: null })
            .then(response => {
              console.log('Unzipping engine');
              // Assuming engine files we want are at unminified/ in the zip
              let zip = new AdmZip(response.body);
              zip.extractEntryTo('unminified/', 'web/', true, true);
            });
      }

      return getEngine.then(() => { 
        return new Promise((resolve, reject) => { 
          console.log('Testing compiler version '+compilerVersion+', engine version '+engineVersion); 

          if(testedEngineVersions.indexOf(engineVersion) < 0) {
            testedEngineVersions.push(engineVersion);
          }

          // We span a new node instance because we dynamically generate the
          // tests each time. We could move the test generation here but this
          // enforces a clean environment more easily

          const spawn = require('child_process').spawn;
          const build = spawn(`node`, [].concat(
            ['test-builder.js', 
            '--compiler-version', compilerVersion, 
            '--engine-version', engineVersion], 
            program.keyboards.length ? ['--keyboards', program.keyboards.join(',')] : []
          ));

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
              fail('Keyboards test failed with code '+code, 3);
              //reject(); // We won't abort because reasons
            }
            resolve();
          });
        });
      });
    });
  }).then(() => {
    console.log('========= Finishing tests =========');
  });

  return testActions;
}).then(() => {
  //
  // Now we need to compare each of the test result files for each keyboard
  //
  console.log('Starting result comparisons');

  let keyboards = util.getKeyboardFolders(KEYBOARDS_ROOT);

  let baseCompilerVersion = testedCompilerVersions.shift(), baseEngineVersion = testedEngineVersions.shift();


  keyboards.forEach((keyboard) => {
    if(!program.keyboards.length || program.keyboards.indexOf(keyboard.s+'/'+keyboard.id) >= 0) {
      // Validate each of the test files against the first tested compiler+engine version
      const baseResultFilename = path.join(KEYBOARDS_ROOT, keyboard.s, keyboard.id, 'tests', `${keyboard.id}-${baseCompilerVersion}-${baseEngineVersion}.results`);
      const baseResult = fs.readFileSync(baseResultFilename, 'utf8');
      //console.log(baseResultFilename, baseResult);
      testedCompilerVersions.forEach((cv) => {
        testedEngineVersions.forEach((ev) => {
          const resultFilename = path.join(KEYBOARDS_ROOT, keyboard.s, keyboard.id, 'tests', `${keyboard.id}-${cv}-${ev}.results`);
          const result = fs.readFileSync(resultFilename, 'utf8');
          //console.log(resultFilename, result);
          if(result != baseResult) {
            fail(`MISMATCH: ${keyboard.s}/${keyboard.id}: compiler: ${baseCompilerVersion}:${cv} and engine: ${baseEngineVersion}:${ev}`, 4);
          }
        });
      });
    }
  });

  if(process.exitCode == 0) {
    console.log('SUCCESS');
  } else {
    console.error('FAILED WITH ERROR '+process.exitCode);
  }
  process.exit();
}).catch((err) => {
  console.error(err);
  process.exit(1);
});
