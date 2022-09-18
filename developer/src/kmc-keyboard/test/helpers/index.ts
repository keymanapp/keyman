/**
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'path';
import * as fs from 'fs';
import { fileURLToPath } from 'url';
import { SectionCompiler } from '../../src/compiler/section-compiler.js';
import KMXPlusFile, { Elem, GlobalSections, Section, Strs } from '../../src/kmx/kmx-plus.js';
import LDMLKeyboardXMLSourceFileReader from '../../src/ldml-keyboard/ldml-keyboard-xml-reader.js';
import { CompilerEvent } from '../../src/compiler/callbacks.js';
import Compiler from '../../src/compiler/compiler.js';
import { assert } from 'chai';
import KMXPlusMetadataCompiler from '../../src/compiler/metadata-compiler.js';
import CompilerOptions from '../../src/compiler/compiler-options.js';

/**
 * Builds a path to the fixture with the given path components.
 *
 * e.g., makePathToFixture('basic.xml')
 *
 * @param components One or more path components.
 */
export function makePathToFixture(...components: string[]): string {
  return fileURLToPath(new URL(path.join('..', '..', '..', 'test', 'fixtures', ...components), import.meta.url));
}


export class CompilerCallbacks {
  messages: CompilerEvent[] = [];
  loadFile(baseFilename: string, filename:string): Buffer {
    // TODO: translate filename based on the baseFilename
    return fs.readFileSync(filename);
  }
  reportMessage(event: CompilerEvent): void {
    this.messages.push(event);
  }
  loadLdmlKeyboardSchema(): Buffer {
    return fs.readFileSync(new URL(path.join('..', '..', 'src', 'ldml-keyboard.schema.json'), import.meta.url));
  }
}

export function loadSectionFixture(compilerClass: typeof SectionCompiler, filename: string, callbacks: CompilerCallbacks): Section {
  callbacks.messages = [];
  const inputFilename = makePathToFixture(filename);
  const source = (new LDMLKeyboardXMLSourceFileReader(callbacks)).loadFile(inputFilename);
  const compiler = new compilerClass(source, callbacks);
  if(!compiler.validate()) {
    return null;
  }

  let globalSections: GlobalSections = {
    strs: new Strs(),
    elem: null
  };
  globalSections.elem = new Elem(globalSections.strs);

  return compiler.compile(globalSections);
}

export function compileKeyboard(inputFilename: string, callbacks: CompilerCallbacks, options: CompilerOptions): KMXPlusFile {
  const k = new Compiler(callbacks, options);
  const source = k.load(inputFilename);
  checkMessages(callbacks);
  assert.isNotNull(source, 'k.load should not have returned null');

  const valid = k.validate(source);
  checkMessages(callbacks);
  assert.isTrue(valid, 'k.validate should not have failed');

  const kmx = k.compile(source);
  checkMessages(callbacks);
  assert.isNotNull(kmx, 'k.compile should not have returned null');

  // In order for the KMX file to be loaded by non-KMXPlus components, it is helpful
  // to duplicate some of the metadata
  KMXPlusMetadataCompiler.addKmxMetadata(kmx.kmxplus, kmx.keyboard, options);

  return kmx;
}

export function checkMessages(callbacks: CompilerCallbacks) {
  if(callbacks.messages.length > 0) {
    console.log(callbacks.messages);
  }
  assert.isEmpty(callbacks.messages);
}
