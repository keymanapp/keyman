#!/usr/bin/env node

/**
 * Implements a simple CLI for interactively testing the given predictive text
 * model.
 */

const path = require('path');
const readline = require('readline');

const {EventIterator} = require('event-iterator');
const program =  require('commander');

// Load the most recent LMLayer code locally.
const LMLayer = require('../../predictive-text');


///////////////////////////////// Constants /////////////////////////////////

const WORKER_DEBUG = false;
const EXIT_USAGE = 1;

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


//////////////////////////////////// main ////////////////////////////////////

main();

function main() {
  // Command line options:
  program
    .version(require('./package.json').version)
    .usage('(-f <model-file> | <model-id>)')
    .description('CLI for trying lexical models.')
    .arguments('[model-id]')
    .option('-f, --model-file <file>', 'path to model file')
    .parse(process.argv);

  let modelFile = determineModelFile(program);

  // Ensure we're running in the terminal
  if (!process.stdin.isTTY) {
    throw new Error('must be run from interactive terminal');
  }

  // Show a quick "how-to" message.
  console.log(unindent(`
    # Testing ${modelFile}
    #
    # Type text in the model's language. Suggestions will appear automatically.
    #
    #  * Press ${ANSI.BOLD}<Tab>${ANSI.NORMAL} to select a suggestion.
    #  * Press ${ANSI.BOLD}<Tab>${ANSI.NORMAL} again to select the next suggestion.
    #  * Press ${ANSI.BOLD}<Shift>${ANSI.NORMAL}+${ANSI.BOLD}<Tab>${ANSI.NORMAL} to select the previous suggestion.
    #  * Press ${ANSI.BOLD}<Enter>${ANSI.NORMAL} to accept the suggestion.
    #  * Press ${ANSI.BOLD}<Ctrl>${ANSI.NORMAL}+${ANSI.BOLD}<C>${ANSI.NORMAL} to quit.
  `))

  // The command line proper handles asynchronous keypresses, hence the rest
  // must also be asynchronous.
  asyncRepl(modelFile)
    .then(_ => process.exit(0))
    .catch(err => {
      console.error(err);
      process.exit(127);
    });
}


async function asyncRepl(modelFile) {
  // Load the LMLayer and the desired model.
  let lm = new LMLayer({}, createAsyncWorker());
  let config = await lm.loadModel(modelFile);

  // Setup the REPL
  // > type some text
  let buffer = '';  // the text to predict on
  let suggestions = [];
  let selectedSuggestionIndex = null;  // which suggestion is currently suggested

  // Draw the prompt for the first time.
  redrawPrompt();

  for await (let [char, keypress] of keypressesFromStdin()) {
    if (wantsQuit(keypress)) {
      process.stdout.write('\n');
      process.exit(0);
    }

    if (keypress.name === 'tab') {
      let n = suggestions.length;
      if (n === 0) {
        // nothing to select.
        selectedSuggestionIndex = null;
        continue;
      }

      // Nothing selected, but there are suggestions -- then select the first
      // one!
      if (selectedSuggestionIndex === null) {
        selectedSuggestionIndex = 0;
        continue;
      }

      if (keypress.shift === false) {
        // Select the next suggestion.
        selectedSuggestionIndex = (selectedSuggestionIndex + 1) % n;
      } else {
        // Select the previous suggestion.

        // Note that JavaScript's modulous operator is annoying with negative
        // numbers, so use an if statement to handle wrapping.
        selectedSuggestionIndex = selectedSuggestionIndex - 1;
        // Wrap around to the end.
        if (selectedSuggestionIndex < 0) {
          selectedSuggestionIndex = n - 1;
        }
      }

      renderSuggestions(suggestions, selectedSuggestionIndex);
    } else if (keypress.name === 'return') {
      // Accept the currently selected
      let acceptedSuggestion = suggestions[selectedSuggestionIndex];

      if (!acceptedSuggestion) {
        // There's no suggestion to insert!
        continue;
      }

      applyTransformToActiveBuffer(acceptedSuggestion.transform);
      redrawPrompt();

      // Ask for suggestions.
      suggestions = await askForPredictions();
      renderSuggestions(suggestions, selectedSuggestionIndex);

    } else if (keypress.name === 'backspace') {
      if (buffer.length === 0) {
        // nothing to do if the buffer is empty
        continue;
      }

      // Remove the last character.
      deleteLastCodepoint();
      redrawPrompt();

      // Ask for suggestions again
      suggestions = await askForPredictions();
      renderSuggestions(suggestions, selectedSuggestionIndex);

    } else {
      // Handle a keypress of a letter or symbol.
      let {transform, context} = insertCharacter(char);
      redrawPrompt();
      suggestions = await askForPredictions(transform, context);
      renderSuggestions(suggestions, selectedSuggestionIndex);
    }
  }

  // Helpers

  /**
   * Asks the LMLayer for predictions with the given transform and context.
   * Asynchronously returns the new suggestions.
   */
  async function askForPredictions(transform = nullTransform(), context = currentContext()) {
    return Array.from(await lm.predict(transform, context));
  }

  /**
   * Returns the null transform.
   */
  function nullTransform() {
    return { insert: '', deleteLeft: 0 };
  }

  /**
   * Returns the current state of the buffer.
   */
  function currentContext() {
    return {
      left: buffer,
      startOfBuffer: buffer.length === 0,
      endOfBuffer: true
    };
  }

  /**
   * Draws the prompt, clearing what was on the screen.
   */
  function redrawPrompt() {
    // Erases the ENTIRE line, returning to the beginning of the line.
    process.stdout.write(ANSI.ERASE_IN_LINE() + '\r');
    // Write the buffer!
    process.stdout.write(`> ${buffer}`);
  }

  /**
   * Inserts a character at the end of the buffer.  Returns the transform and
   * context of the character insert.
   *
   * Note: this mutates the buffer!
   */
  function insertCharacter(char) {
    let oldBuffer = buffer;
    buffer += char;

    let transform = {
      insert: char,
      deleteLeft: 0
    };

    let context = {
      left: oldBuffer,
      startOfBuffer: oldBuffer.length === 0 ? true : false,
      endOfBuffer: true,
    };

    return { transform, context };
  }

  /**
   * Mutates the buffer by removing the last code point. This handles
   * surrogate pairs.
   */
  function deleteLastCodepoint() {
    let lastIndex = buffer.length - 1;
    // When the last character is a low surrogate (0xDC00-0xDFFF), this means
    // we have a surrogate pair! We must delete two 16-bit code units.
    if ('\uDC00' <= buffer[lastIndex] && buffer[lastIndex] <= '\uDFFF') {
      console.assert('\uD800'  <= buffer[lastIndex - 1] && buffer[lastIndex - 1] <= '\uDBFF');
      lastIndex = lastIndex - 1;
    }
    buffer = buffer.substr(0, lastIndex);
  }

  /**
   * Mutates buffer by applying the given transform.
   */
  function applyTransformToActiveBuffer(transform) {
    buffer = buffer.substr(0, buffer.length - transform.deleteLeft) + transform.insert;
  }
}

/**
 * Render the given suggestions on the next line after the cursor.
 * Looks like this:
 *
 *    [suggestions] [appear] [here]
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
        return `[${displayAs}]`
      }).join(' ');
    process.stdout.write(line);
  }

  process.stdout.write(ANSI.RESTORE_CURSOR_POSITION);
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


///////////////////////////////// Utilities /////////////////////////////////

/**
 * Figure out the path to the model file from the command line.
 */
function determineModelFile(program) {
  // invoked as: predictive-text -f path/to/model.js
  if (program.modelFile) {
    return program.modelFile;
  }

  // invoked as: predictive-text author.bcp47.uniq
  // will lookup the model in using LMPATH.
  if (program.args[0]) {
    // Find the model file under the LMPath.
    let modelID = program.args[0];
    let lmPath = process.env['LMPATH'];
    let [author, bcp47, uniq] = modelID.split('.');

    let LMPATH = process.env['LMPATH'];
    if (!LMPATH) {
      usageError('Environment variable LMPATH undefined!');
    }

    return path.join(LMPATH, author, `${bcp47}.${uniq}`, 'build', `${modelID}.model.js`);
  }

  // A model is not specified in any way on the command line. Error out!
  usageError('You did not specify a model!');
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

/**
 * Loges messages that pass between the top-level and the worker, only when
 * WORKER_DEBUG is enabled.
 */
function logInternalWorkerMessage(role, ...args) {
  if (WORKER_DEBUG) {
    console.log(`[${role}]`, ...args);
  }
}

/**
 * Unindents a template literal.
 */
function unindent(string) {
  let lines = string.split('\n');
  // Remove empty lines from the top and bottom.
  if (lines[0] === '') {
    lines.shift();
  }
  if (lines[lines.length -1] === '') {
    lines.pop();
  }

  let numLeadingSpaces = lines[0].match(/^ */)[0].length;

  return lines.map(line => line.substr(numLeadingSpaces)).join('\n');
}

/**
 * Call to quit the program with an error due to incorrect command line usage.
 */
function usageError(message) {
  console.error(`${program.name()}: ${message}\n`);
  program.outputHelp();
  process.exit(EXIT_USAGE);
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
