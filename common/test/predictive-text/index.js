/**
 * Implements a simple CLI for interactively testing the given predictive text
 * model.
 */
const LMLayer = require('../../predictive-text');

const WORKER_DEBUG = false;

/** "Control sequence introducer" for ANSI escape codes: */
const CSI = '\033[';
/**
 * ANSI escape codes to deal with the screen and the cursor.
 * See https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_sequences
 */
const ANSI = {
  CURSOR_NEXT_LINE: CSI + 'E', // Bring cursor to the beginning of the next line
  ERASE_IN_LINE(n=0) { return CSI + n + 'k'; }, // Erase from the cursor to the end of the line
  SAVE_CURSOR_POSITION: CSI + 's', // Remembers the current cursor position
  RESTORE_CURSOR_POSITION: CSI + 'u', // Moves the cursor to the previously stored position.
};

asyncMain()
  .then(_ => process.exit(0))
  .catch(err => {
    console.error(err);
    process.exit(127);
  });


async function asyncMain() {
  let lm = new LMLayer({}, createAsyncWorker());
  let config = await lm.loadModel('./example.crk.wordlist_wahkohtowin.model.js');

  // TODO: a REPL of sorts here:
  //
  // > type some text
  // [suggestions] [appear] [here] (press tab to accept)

  // Initial line:
  process.stdout.write(`> ${ANSI.SAVE_CURSOR_POSITION}`);

  // Proceed to next line to write suggestions.
  process.stdout.write(ANSI.CURSOR_NEXT_LINE + ANSI.ERASE_IN_LINE());

  let suggestions = await lm.predict(insertCharacter('n'), getCurrentContext());

  // Format the displayed suggestions.
  let names = Array.from(suggestions)
    .map(({displayAs}) => `[ ${displayAs} ]`)
    .join(' ');
  process.stdout.write(names);
  process.stdout.write(ANSI.RESTORE_CURSOR_POSITION);
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
  if (WORKER_DEBUG) {
    console.log(`[${role}]`, ...args);
  }
}
