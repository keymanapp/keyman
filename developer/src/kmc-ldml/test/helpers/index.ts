/**
 * Helpers and utilities for the Mocha tests.
 */
import 'mocha';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { SectionCompiler } from '../../src/compiler/section-compiler.js';
import { KMXPlus, LDMLKeyboardXMLSourceFileReader, VisualKeyboard, CompilerEvent, LDMLKeyboardTestDataXMLSourceFile } from '@keymanapp/common-types';
import Compiler from '../../src/compiler/compiler.js';
import { assert } from 'chai';
import KMXPlusMetadataCompiler from '../../src/compiler/metadata-compiler.js';
import CompilerOptions from '../../src/compiler/compiler-options.js';
import VisualKeyboardCompiler from '../../src/compiler/visual-keyboard-compiler.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

import KMXPlusFile = KMXPlus.KMXPlusFile;
import Elem = KMXPlus.Elem;
import GlobalSections = KMXPlus.GlobalSections;
import Section = KMXPlus.Section;
import Strs = KMXPlus.Strs;
import List = KMXPlus.List;

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

export const compilerTestCallbacks = new TestCompilerCallbacks();

beforeEach(function() {
  compilerTestCallbacks.clear();
});

afterEach(function() {
  if (this.currentTest.state !== 'passed') {
    compilerTestCallbacks.messages.forEach(message => console.log(message.message));
  }
});


export async function loadSectionFixture(compilerClass: typeof SectionCompiler, filename: string, callbacks: TestCompilerCallbacks): Promise<Section> {
  callbacks.messages = [];
  const inputFilename = makePathToFixture(filename);
  const data = callbacks.loadFile(inputFilename);
  assert.isNotNull(data);

  const reader = new LDMLKeyboardXMLSourceFileReader(callbacks);
  const source = reader.load(data);
  assert.isNotNull(source);

  if (!reader.validate(source, callbacks.loadSchema('ldml-keyboard'))) {
    return null; // mimic kmc behavior - bail if validate fails
  }

  const compiler = new compilerClass(source, callbacks);

  assert.ok(await compiler.init(), `${compiler.id} failed init()`);

  if(!compiler.validate()) {
    return null;
  }

  let globalSections: GlobalSections = {
    strs: new Strs(),
    elem: null,
    list: null,
    vars: null,
  };
  globalSections.elem = new Elem(globalSections.strs);
  globalSections.list = new List(globalSections.strs);
  globalSections.vars = null; // new Vars(globalSections.strs);

  return compiler.compile(globalSections);
}

export function loadTestdata(inputFilename: string, options: CompilerOptions) : LDMLKeyboardTestDataXMLSourceFile {
  const k = new Compiler(compilerTestCallbacks, options);
  const source = k.loadTestData(inputFilename);
  return source;
}

export async function compileKeyboard(inputFilename: string, options: CompilerOptions): Promise<KMXPlusFile> {
  const k = new Compiler(compilerTestCallbacks, options);
  const source = k.load(inputFilename);
  checkMessages();
  assert.isNotNull(source, 'k.load should not have returned null');

  const valid = k.validate(source);
  checkMessages();
  assert.isTrue(valid, 'k.validate should not have failed');

  const kmx = await k.compile(source);
  checkMessages();
  assert.isNotNull(kmx, 'k.compile should not have returned null');

  // In order for the KMX file to be loaded by non-KMXPlus components, it is helpful
  // to duplicate some of the metadata
  KMXPlusMetadataCompiler.addKmxMetadata(kmx.kmxplus, kmx.keyboard, options);

  return kmx;
}

export function compileVisualKeyboard(inputFilename: string, options: CompilerOptions): VisualKeyboard.VisualKeyboard {
  const k = new Compiler(compilerTestCallbacks, options);
  const source = k.load(inputFilename);
  checkMessages();
  assert.isNotNull(source, 'k.load should not have returned null');

  const valid = k.validate(source);
  checkMessages();
  assert.isTrue(valid, 'k.validate should not have failed');

  const vk = (new VisualKeyboardCompiler()).compile(source);
  checkMessages();
  assert.isNotNull(vk, 'VisualKeyboardCompiler.compile should not have returned null');

  return vk;
}

export function checkMessages() {
  if(compilerTestCallbacks.messages.length > 0) {
    console.log(compilerTestCallbacks.messages);
  }
  assert.isEmpty(compilerTestCallbacks.messages);
}

export interface CompilationCase {
  /**
   * path to xml, such as 'sections/layr/invalid-case.xml'
   */
  subpath: string;
  /**
   * expected error messages. If falsy, expected to succeed. All must be present to pass.
   */
  errors?: CompilerEvent[];
  /**
   * expected warning messages. All must be present to pass.
   */
  warnings?: CompilerEvent[];
  /**
   * optional callback with the section
   */
  callback?: (sect: KMXPlus.Section, subpath: string, callbacks: TestCompilerCallbacks ) => void;
  /**
   * if present, expect compiler to throw (use .* to match all)
   */
  throws?: RegExp;
}

/**
 * Run a bunch of cases
 * @param cases cases to run
 * @param compiler argument to loadSectionFixture()
 * @param callbacks argument to loadSectionFixture()
 */
export function testCompilationCases(compiler: typeof SectionCompiler, cases : CompilationCase[]) {
  // we need our own callbacks rather than using the global so messages don't get mixed
  const callbacks = new TestCompilerCallbacks();
  for (let testcase of cases) {
    const expectFailure = testcase.throws || !!(testcase.errors); // if true, we expect this to fail
    const testHeading = expectFailure ? `should fail to compile: ${testcase.subpath}`:
                                        `should compile: ${testcase.subpath}`;
    it(testHeading, async function () {
      callbacks.clear();
      // special case for an expected exception
      if (testcase.throws) {
        assert.throws(async () => await loadSectionFixture(compiler, testcase.subpath, callbacks), testcase.throws, 'expected exception from compilation');
        return;
      }
      let section = await loadSectionFixture(compiler, testcase.subpath, callbacks);
      if (expectFailure) {
        assert.isNull(section, 'expected compilation result to be null, but got something');
      } else {
        assert.isNotNull(section, `expected successful compilation, but got null and ${JSON.stringify(callbacks.messages)}`);
      }

      // if we expected errors or warnings, show them
      if (testcase.errors) {
        assert.includeDeepMembers(callbacks.messages, testcase.errors, 'expected errors to be included');
      }
      if (testcase.warnings) {
        assert.includeDeepMembers(callbacks.messages, testcase.warnings, 'expected warnings to be included');
      } else if (!expectFailure) {
        // no warnings, so expect zero messages
        assert.strictEqual(callbacks.messages.length, 0, 'expected zero messages');
      }

      // run the user-supplied callback if any
      if (testcase.callback) {
        testcase.callback(section, testcase.subpath, callbacks);
      }
    });
  }
}
