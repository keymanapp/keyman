// test.js

// This is the primary framework for running regression tests

const fs = require('fs-extra');
const path = require('path');
const got = require('got');
const AdmZip = require('adm-zip');
const program = require('commander');
const rimraf = require('rimraf');

// Local modules

const config = require('./node_src/config.js');
const util = require('./node_src/util.js');
const keyname = require('./node_src/keyname.js');

function list(val) {
  return val.split(',');
}

program
  .version('0.1')
  .option('-c, --compiler-versions [versions]', 'Specify compiler version(s) to test. Can specify "stable", "source" or a specific version number.', list, ['stable','source'])
  .option('-e, --engine-versions [versions]', 'Specify KeymanWeb engine version(s) to test. Can specify "stable", "source" or a specific version number.', list, ['stable','source'])
  .option('-k, --keyboards [keyboards]', 'Builds and tests specific keyboard source files. If -k is not specified, then test all keyboards in the keyboards repository.', list, [])
  .option('-f, --fail-fast', "Don't attempt to continue tests after the first keyboard test fails")
  .option('--deep', "Compare all version combinations against base version, instead of just one; only valid when comparing 1 version of each against base")
  .option('--skip-analysis', "Don't create .tests files, assume they are already present")
  .option('-l, --log-all-failures', "Log all test failures to console, not just the first failure for each keyboard");

program.parse(process.argv);

if(program.deep && (program.compilerVersions.length != 2 || program.engineVersions.length != 2)) {
  console.error('Cannot specify --deep unless precisely 1 version is being tested against another for each of compiler, engine versions');
  process.exit(1);
}

if(program.args.length) {
  console.log('Invalid parameters. Run with -h for help');
  process.exit(1);
}

const KEYBOARDS_ROOT = path.join(config.KEYBOARDS_ROOT, config.KEYBOARDS_GROUP);

process.exitCode = 0;

// Environment

// TODO read env var GIT_BASH_FOR_KEYMAN? "C:\Program Files\Git\bin\bash.exe" --init-file "c:\Program Files\Git\etc\profile" -l
const bash = process.platform == 'win32'
  ? { command: 'C:\\Program Files\\Git\\bin\\bash.exe', params: ['--init-file', 'C:\\Program Files\\Git\\etc\\profile', '-l'] }
  : { command: 'bash', params: [] };

//TODO: support testing standalone keyboards that are not in the repo

const compilerDestPath = path.join(config.KEYBOARDS_ROOT, 'tools');
const kmcompDestPath = path.join(compilerDestPath, 'kmcomp.exe');
const kmcmpdllDestPath = path.join(compilerDestPath, 'kmcmpdll.dll');

let testedCompilerVersions = [], testedEngineVersions = [], firstCompile = true;

// Build keyboards -- get kmcomp.exe, kmcmpdll.dll from appropriate location

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

// 
// Clean the keyboards repo -- unless we are skipping analysis
// This removes the build/ and tests/ folders
//

let cleanKeyboards = Promise.resolve();

if(!program.skipAnalysis) {
  console.log('Cleaning keyboards repo');
  let cleanKeyboard = function(keyboard) {
    keyboard = keyboard ? config.KEYBOARDS_GROUP+'/'+keyboard : config.KEYBOARDS_GROUP;
    return util.runProcess(
      `${bash.command}`, 
      [].concat(bash.params, ['build.sh', '-c', keyboard]), 
      { cwd: config.KEYBOARDS_ROOT });
  };
  
  cleanKeyboards = (program.keyboards.length) 
    ? forEachPromise(program.keyboards, cleanKeyboard)
    : cleanKeyboard('');
} 

cleanKeyboards.then(() => {
  fs.renameSync(kmcompDestPath, kmcompDestPath + '.bak');
  fs.renameSync(kmcmpdllDestPath, kmcmpdllDestPath + '.bak');
  
  process.on('exit', () => {
    console.log('Restoring original compiler files');
    fs.renameSync(kmcompDestPath + '.bak', kmcompDestPath);
    fs.renameSync(kmcmpdllDestPath + '.bak', kmcmpdllDestPath);
  });
}).then(() => forEachPromise(program.compilerVersions, version => {
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
      keyboard = keyboard ? config.KEYBOARDS_GROUP+'/'+keyboard : config.KEYBOARDS_GROUP;

      console.log('building keyboard '+keyboard);

      return util.runProcess(
        `${bash.command}`, 
        [].concat(bash.params, ['build.sh', /*TODO: waiting on keyboards repo support for this param: '-T', 'kmn',*/ keyboard]), 
        { cwd: config.KEYBOARDS_ROOT });
    };
  
    return (program.keyboards.length) ? forEachPromise(program.keyboards, buildKeyboard) : buildKeyboard('');
  });

  //
  // TODO: Analyze phase (for first compile run only)
  // TODO: use the js-target kmx intermediate
  //

  let runAnalyze = runCompile.then(() => {
    // If we are first run, then build the .tests files
    if(firstCompile && !program.skipAnalysis) {
      firstCompile = false;
      let analyzeKeyboard = function(keyboard) {
        const locator = keyboard.shortname+'/'+keyboard.id;
  
        const kmx = path.join(config.KEYBOARDS_ROOT, config.KEYBOARDS_GROUP, locator, 'build', keyboard.id+'.kmx');
        const testsPath = path.join(config.KEYBOARDS_ROOT, config.KEYBOARDS_GROUP, locator, 'tests');
        const tests = path.join(testsPath, keyboard.id+'.tests');

        if(!fs.existsSync(kmx)) {
          // TODO: kmanalyze should really call the compiler to build an intermediate .kmx
          // for use with web. This should not be that hard to do...
          console.log('Cannot generate tests at present without a .kmx for '+keyboard);
          return true;
        }
        fs.mkdirSync(path.join(config.KEYBOARDS_ROOT, config.KEYBOARDS_GROUP, locator, 'tests'));

        // TODO: Find kmanalyze outside the repo. This forces Windows-dependence right now
        return util.runProcess('../../../windows/bin/developer/kmanalyze.exe', [kmx, tests]);
      };
    
      let keyboards = program.keyboards.length 
        ? program.keyboards.map((locator) => util.parseLocator(locator)) 
        : util.getKeyboardFolders(KEYBOARDS_ROOT, false);
      return forEachPromise(keyboards, analyzeKeyboard);
    }
  });

  //
  // Test phase, multiple tests to run
  //

  let testActions = runAnalyze.then(() => {
    console.log('========= Starting tests =========');
    return forEachPromise(program.engineVersions, (version0) => {

      if(!program.deep) {
        // We don't do complete pairwise tests unless --deep is specified
        if((version0 == program.engineVersions[0] && version == program.compilerVersions[1]) ||
        (version0 == program.engineVersions[1] && version == program.compilerVersions[0])) {
          console.log('Skipping compiler version '+version0+', engine version '+version);
          return true;
        }
      }

      let getEngine, engineVersion;
      switch(version0) {
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
          console.log('Downloading specified engine version '+version0);
          engineVersion = version0;
          getEngine = got(`https://downloads.keyman.com/web/stable/${version0}/keymanweb-${version0}.zip`, { encoding: null })
            .then(response => {
              console.log('Unzipping engine');
              // Assuming engine files we want are at unminified/ in the zip
              let zip = new AdmZip(response.body);
              zip.extractEntryTo('unminified/', 'web/', true, true);
            });
      }

      return getEngine.then(() => { 
        console.log('Testing compiler version '+compilerVersion+', engine version '+engineVersion); 

        if(testedEngineVersions.indexOf(engineVersion) < 0) {
          testedEngineVersions.push(engineVersion);
        }

        return util.runProcess(
          `node`,
          [].concat(
            ['test-builder.js', 
            '--compiler-version', compilerVersion, 
            '--engine-version', engineVersion], 
            program.keyboards.length ? ['--keyboards', program.keyboards.join(',')] : []
          )
        );
      });
    });
  }).then(() => {
    console.log('========= Finishing tests =========');
  });

  return testActions;
})).then(() => {
  //
  // Now we need to compare each of the test result files for each keyboard
  //
  console.log('Starting result comparisons');

  let keyboards = util.getKeyboardFolders(KEYBOARDS_ROOT, true);

  let baseCompilerVersion = testedCompilerVersions.shift(), baseEngineVersion = testedEngineVersions.shift();

  keyboards.forEach((keyboard) => {
    if(!program.keyboards.length || program.keyboards.indexOf(keyboard.shortname+'/'+keyboard.id) >= 0) {
      // Validate each of the test files against the first tested compiler+engine version
      const baseResultFilename = path.join(KEYBOARDS_ROOT, keyboard.shortname, keyboard.id, 'tests', `${keyboard.id}-${baseCompilerVersion}-${baseEngineVersion}.results`);
      const baseResult = fs.readFileSync(baseResultFilename, 'utf8');
      const baseResultJSON = JSON.parse(baseResult);

      const testsFilename = path.join(KEYBOARDS_ROOT, keyboard.shortname, keyboard.id, 'tests', `${keyboard.id}.tests`);
      const testsJSON = JSON.parse(fs.readFileSync(testsFilename, 'utf8'));
      //console.log(baseResultFilename, baseResult);
      testedCompilerVersions.forEach((cv) => {
        testedEngineVersions.forEach((ev) => {
          const resultFilename = path.join(KEYBOARDS_ROOT, keyboard.shortname, keyboard.id, 'tests', `${keyboard.id}-${cv}-${ev}.results`);
          const result = fs.readFileSync(resultFilename, 'utf8');
          //console.log(resultFilename, result);
          // Naive string test first
          if(result !== baseResult) {
            // Now, report first mismatch and total number of mismatches after parsing JSON
            const resultJSON = JSON.parse(result);
            let errors = 0, prefix = `${keyboard.shortname}/${keyboard.id}`;
            for(let k in baseResultJSON) {
              if(resultJSON[k] !== baseResultJSON[k]) {
                if(++errors == 1 || program.logAllFailures) {
                  let 
                    ix = k.toString(), 
                    whitespace = ' '.repeat(prefix.length + ix.length + 6),
                    input = `${testsJSON.inputTests[k].context ? `"${testsJSON.inputTests[k].context}" ` : ""}+ ${keyname(testsJSON.inputTests[k].modifier, testsJSON.inputTests[k].key)}`;
                  console.error(`${prefix}[${ix}]: expected: ${input} > "${baseResultJSON[k]}"`);
                  console.error(`${whitespace}actual: ${input} > "${resultJSON[k]}"`);
                }
              }
            }
            fail(`${keyboard.shortname}/${keyboard.id} ${errors} test(s) mismatched between (${baseCompilerVersion} / ${baseEngineVersion}) and (${cv} / ${ev})`, 4);
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
