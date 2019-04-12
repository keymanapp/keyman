/**
 * Implements a simple CLI for interactively testing the given predictive text
 * model.
 */

const readline = require('readline');

const {EventIterator} = require('event-iterator');


// Load the most recent LMLayer code locally.
const LMLayer = require('../../predictive-text');

const WORKER_DEBUG = false;

/** "Control sequence introducer" for ANSI escape codes: */
const CSI = '\033[';
/**
 * ANSI escape codes to deal with the screen and the cursor.
 * See https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_sequences
 */
const ANSI = {
  CURSOR_NEXT_LINE: CSI + 'E', // Bring cursor to the beginning of the next line.
  ERASE_IN_LINE(n=2) { return CSI + n + 'K'; }, // Erase the entire line.
  SAVE_CURSOR_POSITION: CSI + 's', // Remembers the current cursor position.
  RESTORE_CURSOR_POSITION: CSI + 'u', // Moves the cursor to the previously stored position.
  BOLD: CSI + '1m', // Set bold text.
  REVERSE_VIDEO: CSI + '7m', // Invert the text colour (swap background and foreground colours).
  NORMAL_VIDEO: CSI + '27m', // Uninvert the text colour (swap background and foreground colours).
  NORMAL: CSI + 'm', // Set all graphics renditions attributes off.
};

// Do it!
main();

function main() {
  // Ensure we're running in the terminal
  if (!process.stdin.isTTY) {
    throw new Error('must be run from interactive terminal');
  }

  asyncMain()
    .then(_ => process.exit(0))
    .catch(err => {
      console.error(err);
      process.exit(127);
    });
}


async function asyncMain() {
  // Load the LMLayer and the desired model.
  let lm = new LMLayer({}, createAsyncWorker());
  let config = await lm.loadModel('./example.crk.wordlist_wahkohtowin.model.js');

  // Setup the REPL
  // > type some text
  // [suggestions] [appear] [here] (press tab to accept)

  // Initial line:
  process.stdout.write(`> `);

  for await (let [char, keypress] of keypressesFromStdin()) {
    if (wantsQuit(keypress)) {
      process.stdout.write('\n');
      process.exit(0);
    } else {
      process.stdout.write(char);
      let suggestions = Array.from(await lm.predict(insertCharacter(char), getCurrentContext()));
      renderSuggestions(suggestions, 0);
    }
  }
}

/**
 * Render the given suggestions on the next line after the cursor.
 */
function renderSuggestions(suggestions, selected) {
  // Wherever we are, save the current cursor position.
  process.stdout.write(ANSI.SAVE_CURSOR_POSITION);

  // Jump to next line and erase it to write suggestions.
  process.stdout.write(ANSI.CURSOR_NEXT_LINE + ANSI.ERASE_IN_LINE());

  if (!suggestions || suggestions.length === 0) {
    process.stdout.write(' no suggestions ');
  } else {
    // Format the displayed suggestions.
    let line = suggestions
      .map(({displayAs}, index) => {
        if (index === selected) {
          return `${ANSI.REVERSE_VIDEO}[${displayAs}]${ANSI.NORMAL_VIDEO}`;
        }
        return `[ ${displayAs} ]`
      }).join(' ');
    process.stdout.write(line);
  }

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

/**
 * True if the keypress looks like something that typically quits the program.
 */
function wantsQuit(keypress) {
  const CTRL_C = '\u0003';
  const CTRL_D = '\u0004';
  const CTRL_BACKSLASH = '\u001C';
  let code = keypress.sequence;
  return code === CTRL_C || code === CTRL_D || code === CTRL_BACKSLASH;
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

/**
 * Allows you to iterate over each keypress event from stdin.
 *
 * Each iteration will yield a two-valued array of [char, keypress].
 * `char` is the string emitted by the keypress. This MAY be undefined!
 * `keypress` is an object with the following properties:
 *
 *    sequence: string; // the raw sequence recieved by the terminal driver.
 *    name: string;     // name of the primary key pressed
 *    ctrl: bool;       // was ctrl pressed?
 *    meta: bool;       // was meta (Command/Windows key) pressed?
 *    shift: bool;      // was shift pressed?
 */
function keypressesFromStdin() {
  let stream = process.stdin;

  readline.emitKeypressEvents(stream);
  if (stream.isTTY) {
    stream.setRawMode(true);
  }
  stream.resume();

  return new EventIterator(
    (push, stop, fail) => {
      stream.on('keypress', (char, keypress) => push([char, keypress]));
      stream.on('close', stop);
      stream.on('error', fail);
    }
  );
}
