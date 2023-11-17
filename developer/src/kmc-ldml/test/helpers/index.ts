/**
 * Helpers and utilities for the Mocha tests.
 */
import 'mocha';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { SectionCompiler, SectionCompilerNew } from '../../src/compiler/section-compiler.js';
import { KMXPlus, LDMLKeyboardXMLSourceFileReader, VisualKeyboard, CompilerEvent, LDMLKeyboardTestDataXMLSourceFile, compilerEventFormat, LDMLKeyboard, UnicodeSetParser, CompilerCallbacks } from '@keymanapp/common-types';
import { LdmlKeyboardCompiler } from '../../src/main.js'; // make sure main.js compiles
import { assert } from 'chai';
import { KMXPlusMetadataCompiler } from '../../src/compiler/metadata-compiler.js';
import { LdmlCompilerOptions } from '../../src/compiler/ldml-compiler-options.js';
import { LdmlKeyboardVisualKeyboardCompiler } from '../../src/compiler/visual-keyboard-compiler.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

import KMXPlusFile = KMXPlus.KMXPlusFile;
import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
import DependencySections = KMXPlus.DependencySections;
import Section = KMXPlus.Section;
import { ElemCompiler, ListCompiler, StrsCompiler } from '../../src/compiler/empty-compiler.js';
import { KmnCompiler } from '@keymanapp/kmc-kmn';
import { VarsCompiler } from '../../src/compiler/vars.js';
// import Vars = KMXPlus.Vars;

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

export const compilerTestOptions: LdmlCompilerOptions = {
  readerOptions: {
    importsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL))
  }
};

beforeEach(function() {
  compilerTestCallbacks.clear();
});

afterEach(function() {
  if (this.currentTest.state !== 'passed') {
    compilerTestCallbacks.messages.forEach(message => console.log(message.message));
  }
});


export async function loadSectionFixture(compilerClass: SectionCompilerNew, filename: string, callbacks: TestCompilerCallbacks, dependencies?: SectionCompilerNew[], postValidateFail?: boolean): Promise<Section> {
  callbacks.messages = [];
  const inputFilename = makePathToFixture(filename);
  const data = callbacks.loadFile(inputFilename);
  assert.isNotNull(data, `Failed to read file ${inputFilename}`);

  const reader = new LDMLKeyboardXMLSourceFileReader(compilerTestOptions.readerOptions, callbacks);
  const source = reader.load(data);
  assert.isNotNull(source, `Failed to load XML from ${inputFilename}`);

  if (!reader.validate(source)) {
    return null; // mimic kmc behavior - bail if validate fails
  }

  const compiler = new compilerClass(source, callbacks);

  if(!compiler.validate()) {
    return null;
  }

  let sections: DependencySections = {
    usetparser: await getTestUnicodeSetParser(callbacks)
  };

  // load dependencies first
  await loadDepsFor(sections, compiler, source, callbacks, dependencies);

  // make sure all dependencies are loaded
  compiler.dependencies.forEach(dep => assert.ok(sections[dep],
      `Required dependency '${dep}' for '${compiler.id}' was not supplied: Check the 'dependencies' argument to loadSectionFixture or testCompilationCases`));

  const section = await compiler.compile(sections);
  const postValidate = compiler.postValidate(section);
  assert.equal(postValidate, !postValidateFail, `expected postValidate() to return ${!postValidateFail}`);
  return section;
}

/**
 * Recursively load dependencies. Normally they are loaded in SECTION_COMPILERS order
 */
async function loadDepsFor(sections: DependencySections, parentCompiler: SectionCompiler, source: LDMLKeyboardXMLSourceFile, callbacks: TestCompilerCallbacks, dependencies?: SectionCompilerNew[]) {
  const parentId = parentCompiler.id;
  if (!dependencies) {
    // default dependencies
    dependencies = [ StrsCompiler, ListCompiler, ElemCompiler, VarsCompiler ];
  }
  for (const dep of dependencies) {
    const compiler = new dep(source, callbacks);
    assert.notEqual(compiler.id, parentId, `${parentId} depends on itself`);
    assert.ok(compiler.validate(), `while setting up ${parentId}: ${compiler.id} failed validate()`);

    const sect = compiler.compile(sections);

    assert.ok(sect, `while setting up ${parentId}: ${compiler.id} failed compile()`);
    assert.notOk(sections[compiler.id], `while setting up ${parentId}: ${compiler.id} was already in the sections[] table, probably a bad dependency`);

    sections[compiler.id] = sect as any;
  }
}

export function loadTestdata(inputFilename: string, options: LdmlCompilerOptions) : LDMLKeyboardTestDataXMLSourceFile {
  const k = new LdmlKeyboardCompiler(compilerTestCallbacks, options);
  const source = k.loadTestData(inputFilename);
  return source;
}

export async function compileKeyboard(inputFilename: string, options: LdmlCompilerOptions): Promise<KMXPlusFile> {
  const k = new LdmlKeyboardCompiler(compilerTestCallbacks, options);
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

export function compileVisualKeyboard(inputFilename: string, options: LdmlCompilerOptions): VisualKeyboard.VisualKeyboard {
  const k = new LdmlKeyboardCompiler(compilerTestCallbacks, options);
  const source = k.load(inputFilename);
  checkMessages();
  assert.isNotNull(source, 'k.load should not have returned null');

  const valid = k.validate(source);
  checkMessages();
  assert.isTrue(valid, 'k.validate should not have failed');

  const vk = (new LdmlKeyboardVisualKeyboardCompiler(compilerTestCallbacks)).compile(source);
  checkMessages();
  assert.isNotNull(vk, 'LdmlKeyboardVisualKeyboardCompiler.compile should not have returned null');

  return vk;
}

export function checkMessages() {
  if(compilerTestCallbacks.messages.length > 0) {
    console.log(compilerTestCallbacks.messages);
  }
  assert.isEmpty(compilerTestCallbacks.messages, compilerEventFormat(compilerTestCallbacks.messages));
}

export interface CompilationCase {
  /**
   * path to xml, such as 'sections/layr/invalid-case.xml'
   */
  subpath: string;
  /**
   * expected error messages. If falsy, expected to succeed. All must be present to pass.
   */
  errors?: CompilerEvent[] | boolean;
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
  /**
   * Optional dependent sections to load.  Will be strs+list+elem if falsy.
   */
  dependencies?: (SectionCompilerNew)[];
  /**
   * Optional, if true, postValidate() must return false. (must be != postValidate())
   */
  postValidateFail?: boolean;
}

/**
 * Run a bunch of cases
 * @param cases cases to run
 * @param compiler argument to loadSectionFixture()
 * @param callbacks argument to loadSectionFixture()
 */
export function testCompilationCases(compiler: SectionCompilerNew, cases : CompilationCase[], dependencies?: (SectionCompilerNew)[]) {
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
        assert.throws(async () => await loadSectionFixture(compiler, testcase.subpath, callbacks, testcase.dependencies || dependencies), testcase.throws, 'expected exception from compilation');
        return;
      }
      let section = await loadSectionFixture(compiler, testcase.subpath, callbacks, testcase.dependencies || dependencies);
      if (expectFailure) {
        assert.isNull(section, 'expected compilation result failure (null)');
      } else {
        assert.isNotNull(section, `failed with ${compilerEventFormat(callbacks.messages)}`);
      }

      // if we expected errors or warnings, show them
      if (testcase.errors && testcase.errors !== true) {
        assert.includeDeepMembers(callbacks.messages, <CompilerEvent[]> testcase.errors, 'expected errors to be included');
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
async function getTestUnicodeSetParser(callbacks: CompilerCallbacks): Promise<UnicodeSetParser> {
  // for tests, just create a new one
  // see LdmlKeyboardCompiler.getUsetParser()
  const compiler = new KmnCompiler();
  const ok = await compiler.init(callbacks);
  assert.ok(ok, `Could not initialize KmnCompiler (UnicodeSetParser), see callback messages`);
  if (ok) {
    return compiler;
  } else {
    return null;
  }
}

