/**
 *
 */
const LMLayer = require('../../predictive-text');


asyncMain().then(_ => process.exit(0));

async function asyncMain() {
  let lm = new LMLayer({}, createAsyncWorker());
  let config = await lm.loadModel('./example.crk.wordlist_wahkohtowin.model.js');
  let prediction = await lm.predict(insertCharacter('n'), getCurrentContext());
}

/**
 * A transform that inserts a single character.
 */
function insertText(char) {
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
      console.log('[top-level]', message);
      workerScope.onmessage({ data: message });
    },
    onmessage(message) {
      throw new Error('this should be reassigned!');
    }
  };

  let workerScope = {
    LMLayerWorker,
    postMessage(message) {
      console.log('[worker]', message);
      worker.onmessage({ data: message });
    },
    importScripts(uri) {
      let sourceCode = fs.readFileSync(uri, 'UTF-8');
      console.log('[worker] execing file:', uri);
      vm.runInContext(sourceCode, workerScope, {
        filename: uri,
        displayErrors: true,
        breakOnSigint: true,
      });
    }
  };
  vm.createContext(workerScope);

  let internal = LMLayerWorker.install(workerScope);
  LMLayerWorker.loadModel = function (uri) {
    internal.loadModel(uri);
  }

  return worker;
}
