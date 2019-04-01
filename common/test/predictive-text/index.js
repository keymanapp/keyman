/**
 * Implements a simple CLI for interactively testing the given predictive text
 * model.
 */
const LMLayer = require('../../predictive-text');


asyncMain()
  .then(_ => process.exit(0))
  .catch(err => {
    console.error(err);
    process.exit(127);
  })

async function asyncMain() {
  let lm = new LMLayer({}, createAsyncWorker());
  let config = await lm.loadModel('./example.crk.wordlist_wahkohtowin.model.js');

  // TODO: a REPL of sorts here:
  //
  // > type some text
  // [suggestions] [appear] [here] (press tab to accept)

  let suggestions = await lm.predict(insertCharacter('n'), getCurrentContext());
  for (let s of suggestions) {
    console.log('>>>', s);
  }
}

/**
 * A transform that inserts a single character.
 */
function insertCharacter(char) {
  return {
    insert: char,
    deleteLeft: 0
  };
}

/**
 * Gets the current context from the current buffer.
 */
function getCurrentContext() {
  // TODO: base this on an actual buffer!
  return {
    left: '',
    startOfBuffer: true,
    endOfBuffer: true,
  };
}


function createAsyncWorker() {
  // XXX: import the LMLayerWorker directly -- I know where it is built.
  const LMLayerWorker = require('../../predictive-text/build/intermediate');
  const vm = require('vm');
  const fs = require('fs');

  let worker = {
    postMessage(message) {
      logInternalWorkerMessage('top-level', message);
      workerScope.onmessage({ data: message });
    },
    onmessage(message) {
      throw new Error('this should be reassigned!');
    }
  };

  let workerScope = {
    LMLayerWorker,
    postMessage(message) {
      logInternalWorkerMessage('worker', message);
      worker.onmessage({ data: message });
    },
    importScripts(uri) {
      let sourceCode = fs.readFileSync(uri, 'UTF-8');
      logInternalWorkerMessage('worker', 'execing file:', uri);
      vm.runInContext(sourceCode, workerScope, {
        filename: uri,
        displayErrors: true,
        breakOnSigint: true,
      });
    }
  };
  vm.createContext(workerScope);

  let internal = LMLayerWorker.install(workerScope);

  // XXX: The current generated file expects these functions, but they do not,
  //      and will not exist! Stub them out:
  LMLayerWorker['loadWordBreaker'] = function () {
    console.warn('ignoring word breaker');
  };
  LMLayerWorker['loadModel'] = function (uri) {
    // delegate to the internal LMLayerWorker instance:
    internal.loadModel(uri);
  }

  return worker;
}

/**
 * For logging messages that pass between the top-level and the worker.
 */
function logInternalWorkerMessage(role, ...args) {
  console.log(`[${role}]`, ...args);
}
