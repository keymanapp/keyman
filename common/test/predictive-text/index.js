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

  let workerScope = {
    postMessage(message) {
      console.log('[worker]', message);
    },
    importScripts(uri) {
      let sourceCode = fs.readFileSync(uri, 'UTF-8');
      console.log('[worker] execing file:', uri);
      vm.runInContext(sourceCode, workerScope);
    }
  };
  vm.createContext(workerScope);

  let worker = LMLayerWorker.install(workerScope);
  // 
  LMLayerWorker['loadModel'] = function (uri) {
    console.log('Yup, loading this here script...');
    worker.loadModel(uri);
  }

  return {
    postMessage(message) {
      console.log('[top-level]', message);
      workerScope.onmessage({ data: message });
    }
  };
}
