import { dirname } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KmnCompiler } from '@keymanapp/kmc-kmn';
import { KMX, KmxFileReader } from '@keymanapp/common-types';
import { WriteCompiledKeyboard } from '../src/compiler/write-compiled-keyboard.js';
import { extractTouchLayout } from './util.js';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const fixturesDir = __dirname + '/../../test/fixtures/';
const fixtureName = fixturesDir + 'khmer_angkor.js';
const infile = fixturesDir + 'khmer_angkor.kmn';
const outfile = fixturesDir + 'khmer_angkor.kmx'; // intermediate outfile
const testOutfile = fixturesDir + 'khmer_angkor.test.js';

if(fs.existsSync(testOutfile)) {
  fs.unlinkSync(testOutfile);
}

const callbacks = new TestCompilerCallbacks();

const kmnCompiler = new KmnCompiler();
if(!await kmnCompiler.init(callbacks)) {
  console.error('kmx compiler failed to init');
  process.exit(1);
}

// TODO: runToMemory, add option to kmxCompiler to store debug-data for conversion to .js (e.g. store metadata, group readonly metadata, etc)
if(!kmnCompiler.run(infile, outfile, {
  shouldAddCompilerVersion: false,
  saveDebug: true,
  target: 'js'
})) {
  callbacks.printMessages();
  process.exit(1);
}

const reader = new KmxFileReader();
const keyboard: KMX.KEYBOARD = reader.read(callbacks.loadFile(outfile));

const js = WriteCompiledKeyboard(callbacks, infile, outfile, 'khmer_angkor', keyboard, true);

callbacks.printMessages();

const fjs = fs.readFileSync(fixtureName, 'utf8');

const expected = extractTouchLayout(fjs);
const actual = extractTouchLayout(js);

if(expected.js !== actual.js) {
  fs.writeFileSync(testOutfile, js);
  console.error('JS not equal');
  process.exit(1);
}
