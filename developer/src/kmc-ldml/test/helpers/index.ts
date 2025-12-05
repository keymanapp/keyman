/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Helpers and utilities for the Mocha tests.
 */
import 'mocha';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { SectionCompiler, SectionCompilerNew } from '../../src/compiler/section-compiler.js';
import { util, KMXPlus, LdmlKeyboardTypes } from '@keymanapp/common-types';
import { CompilerEvent, compilerEventFormat, CompilerCallbacks, LDMLKeyboardXMLSourceFileReader, LDMLKeyboardTestDataXMLSourceFile, LDMLKeyboard, CompilerError } from "@keymanapp/developer-utils";
import { LdmlKeyboardCompiler } from '../../src/main.js'; // make sure main.js compiles
import { assert } from 'chai';
import { KMXPlusMetadataCompiler } from '../../src/compiler/metadata-compiler.js';
import { LdmlCompilerOptions } from '../../src/compiler/ldml-compiler-options.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

import KMXPlusFile = KMXPlus.KMXPlusFile;
import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
import DependencySections = KMXPlus.DependencySections;
import Section = KMXPlus.Section;
import { ElemCompiler, ListCompiler, StrsCompiler } from '../../src/compiler/empty-compiler.js';
import { KmnCompiler } from '@keymanapp/kmc-kmn';
import { VarsCompiler } from '../../src/compiler/vars.js';
import { KMXPlusVersion } from '@keymanapp/ldml-keyboard-constants';

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
    cldrImportsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL)),
    localImportsPaths: [], // will be fixed up in loadSectionFixture
  }
};

beforeEach(function() {
  compilerTestCallbacks.clear();
});

afterEach(function() {
  if (this.currentTest.state !== 'passed') {
    compilerTestCallbacks.printMessages();
  }
});


export async function loadSectionFixture(compilerClass: SectionCompilerNew, filename: string, callbacks: TestCompilerCallbacks, targetVersion: KMXPlusVersion, dependencies?: SectionCompilerNew[], postValidateFail?: boolean): Promise<Section> {
  callbacks.messages = [];
  const inputFilename = makePathToFixture(filename);
  const data = callbacks.loadFile(inputFilename);
  assert.isNotNull(data, `Failed to read file ${inputFilename}`);

  compilerTestOptions.readerOptions.localImportsPaths =  [ path.dirname(inputFilename) ];

  const reader = new LDMLKeyboardXMLSourceFileReader(compilerTestOptions.readerOptions, callbacks);
  const source = reader.load(data);
  if (!source) {
    // print any callbacks here
    assert.sameDeepMembers(callbacks.messages, [], `Errors loading ${inputFilename}`);
  }
  assert.isNotNull(source, `Failed to load XML from ${inputFilename}`);

  if (!reader.validate(source)) {
    return null; // mimic kmc behavior - bail if validate fails
  }

  const compiler = new compilerClass(source, callbacks, targetVersion);

  if(!compiler.validate()) {
    return null;
  }

  const sections: DependencySections = {
    usetparser: await getTestUnicodeSetParser(callbacks)
  };

  // load dependencies first
  await loadDepsFor(sections, compiler, source, callbacks, dependencies);

  if (callbacks.hasError()) {
    // break out if there's an error
    return null;
  }
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
    const compiler = new dep(source, callbacks, parentCompiler.targetVersion);
    assert.notEqual(compiler.id, parentId, `${parentId} depends on itself`);
    const didValidate = compiler.validate();
    if (!callbacks.hasError()) {
      // only go down this path if there isn't already a noted error
      assert.ok(didValidate, `while setting up ${parentId}: ${compiler.id} failed validate()`);

      const sect = compiler.compile(sections);

      assert.ok(sect, `while setting up ${parentId}: ${compiler.id} failed compile()`);
      assert.notOk(sections[compiler.id], `while setting up ${parentId}: ${compiler.id} was already in the sections[] table, probably a bad dependency`);

      sections[compiler.id] = sect as any;
    }
  }
}

export async function loadTestData(inputFilename: string, options: LdmlCompilerOptions) : Promise<LDMLKeyboardTestDataXMLSourceFile> {
  const k = new LdmlKeyboardCompiler();
  assert.isTrue(await k.init(compilerTestCallbacks, options));
  const source = k.loadTestData(inputFilename);
  return source;
}

export async function compileKeyboard(inputFilename: string, options: LdmlCompilerOptions, validateMessages?: CompilerEvent[], expectFailValidate?: boolean, compileMessages?: CompilerEvent[]): Promise<KMXPlusFile> {
  const k = new LdmlKeyboardCompiler();
  assert.isTrue(await k.init(compilerTestCallbacks, options));
  const source = k.load(inputFilename);
  checkMessages();
  assert.isNotNull(source, 'k.load should not have returned null');

  const valid = await k.validate(source);
  if (validateMessages) {
    assert.sameDeepMembers(scrubContextFromMessages(compilerTestCallbacks.messages), validateMessages, "validation messages mismatch");
    assert.notEqual(valid, expectFailValidate, 'validation failure');
  } else {
    checkMessages();
    assert.isTrue(valid, 'k.validate should not have failed');
  }

  if (!valid) return null; // get out, if the above asserts didn't get us out.

  const kmx = await k.compile(source);
  if (compileMessages) {
    assert.sameDeepMembers(scrubContextFromMessages(compilerTestCallbacks.messages), compileMessages, "compiler messages mismatch");
  } else {
    checkMessages();
  }
  assert.isNotNull(kmx, 'k.compile should not have returned null');

  // In order for the KMX file to be loaded by non-KMXPlus components, it is helpful
  // to duplicate some of the metadata
  KMXPlusMetadataCompiler.addKmxMetadata(kmx.kmxplus, kmx.keyboard, options);

  return kmx;
}

export function checkMessages() {
  assert.isEmpty(compilerTestCallbacks.messages, compilerEventFormat(compilerTestCallbacks.messages));
}

/**
 * Like CompilerEvent, but supports regex matching.
 */
interface CompilerMatch {
  /** code of the event */
  code: number;
  /** regex that matches 'message' */
  matchMessage: RegExp;
};

/** Union type for a CompilerEvent or a CompilerMatch */
export type CompilerEventOrMatch = CompilerEvent | CompilerMatch;

/** @returns true if 'm' matches 'e' */
export function matchCompilerEvent(e: CompilerEvent, m: CompilerMatch): boolean {
  return (e.code === m.code) && (m.matchMessage.test(e.message));
}

/** @returns the first of events which matches m. Or m, if it's a CompilerEvent */
function findMatchingCompilerEvent(events: CompilerEvent[], m: CompilerEventOrMatch): CompilerEventOrMatch {
  const asMatch = <CompilerMatch> m;
  if (!asMatch.matchMessage) {
    // it's not a valid CompilerMatch, just return it
    return m;
  }

  for (const e of events) {
    if (matchCompilerEvent(e, asMatch)) {
      return e;
    }
  }
  return null;
}
/**
 * for any matched events, substitute them with the original CompilerEvent
 * for any non-matching item, leave as is so that tests will fail
 */
export function matchCompilerEvents(events: CompilerEvent[], matches?: CompilerEventOrMatch[]): CompilerEventOrMatch[] {
  // pass through if there's no processing to be done
  if (!events || !matches) return matches;
  return matches.map(e => findMatchingCompilerEvent(events, e) || e);
}

/** as above, but passes through true/false */
function matchCompilerEventsOrBoolean(events: CompilerEvent[], matches?: CompilerEventOrMatch[] | boolean) : CompilerEventOrMatch[] | boolean {
  if(matches === true || matches === false) return matches;
  return matchCompilerEvents(events, matches);
}

export interface CompilationCase {
  /** if true, expect no further errors than what's in errors.  */
  strictErrors?: boolean;
  /**
   * path to xml, such as 'sections/layr/invalid-case.xml'
   */
  subpath: string;
  /**
   * expected error messages. If falsy, expected to succeed. All must be present to pass.
   */
  errors?: CompilerEventOrMatch[] | boolean;
  /**
   * expected warning messages. All must be present to pass.
   */
  warnings?: CompilerEventOrMatch[];
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
  /**
   * retain offset (line number) information. otherwise, scrub it to reduce testing noise.
   * only tests that specifically are checking offsets will set this to true
   */
  retainOffsetInMessages?: boolean;
}

/**
 * Scrub 'context' from messages. to simplify unit tests
 * @param messages input array of messages
 * @returns copy of messages
 */
export function scrubContextFromMessages(messages: CompilerEvent[]): CompilerEvent[] {
  return messages.map(m => {
    const scrubbed = Object.assign({}, m);
    // Turn this on once all messages have offsets, see messages.tests.ts
    // if (!scrubbed.offset) {
    //   throw Error(`Error, no offset detected in message ${CompilerError.formatEvent(m)}`);
    // }
    delete scrubbed.offset;
    delete scrubbed.line;
    delete scrubbed.filename;
    delete scrubbed.column;
    return scrubbed;
  });
}

/**
 * Run a bunch of cases
 * @param cases cases to run
 * @param compiler argument to loadSectionFixture()
 * @param callbacks argument to loadSectionFixture()
 */
export function testCompilationCases(compiler: SectionCompilerNew, cases : CompilationCase[], targetVersion: KMXPlusVersion, dependencies?: (SectionCompilerNew)[]) {
  // we need our own callbacks rather than using the global so messages don't get mixed
  const callbacks = new TestCompilerCallbacks();
  for (const testcase of cases) {
    const expectFailure = testcase.throws || !!(testcase.errors); // if true, we expect this to fail
    const testHeading = expectFailure ? `should fail to compile: ${testcase.subpath}`:
                                        `should compile: ${testcase.subpath}`;
    it(testHeading, async function () {
      callbacks.clear();
      // special case for an expected exception
      if (testcase.throws) {
        assert.throws(async () => await loadSectionFixture(compiler, testcase.subpath, callbacks, targetVersion, testcase.dependencies || dependencies), testcase.throws, 'expected exception from compilation');
        return;
      }
      const section = await loadSectionFixture(compiler, testcase.subpath, callbacks, targetVersion, testcase.dependencies || dependencies);
      let messagesToCheck = callbacks.messages;
      // scrub offsets from messages to reduce churn in the test casws
      if (!testcase.retainOffsetInMessages && callbacks.messages) {
        messagesToCheck = scrubContextFromMessages(callbacks.messages);
      }
      const testcaseErrors = matchCompilerEventsOrBoolean(messagesToCheck, testcase.errors);
      const testcaseWarnings = matchCompilerEvents(messagesToCheck, testcase.warnings);
      // if we expected errors or warnings, show them
      if (testcaseErrors && testcaseErrors !== true) {
        assert.includeDeepMembers(messagesToCheck, <CompilerEventOrMatch[]>testcaseErrors, 'expected errors to be included');
      }
      if (testcaseErrors && testcase.strictErrors) {
        assert.sameDeepMembers(messagesToCheck, <CompilerEventOrMatch[]>testcaseErrors, 'expected same errors to be included');
      }
      if (testcaseWarnings) {
        assert.includeDeepMembers(messagesToCheck, testcaseWarnings, 'expected warnings to be included');
      } else if (!expectFailure) {
        // no warnings, so expect zero messages
        assert.sameDeepMembers(messagesToCheck, [], 'expected zero messages but got ' + callbacks.messages);
      }

      if (expectFailure) {
        assert.isNull(section, 'expected compilation result failure (null)');
      } else {
        assert.isNotNull(section, `failed with ${CompilerError.formatEvent(callbacks.messages)}`);
      }

      // run the user-supplied callback if any
      if (testcase.callback) {
        testcase.callback(section, testcase.subpath, callbacks);
      }
    });
  }
}
async function getTestUnicodeSetParser(callbacks: CompilerCallbacks): Promise<LdmlKeyboardTypes.UnicodeSetParser> {
  // for tests, just create a new one
  // see LdmlKeyboardCompiler.getUsetParser()
  const compiler = new KmnCompiler();
  const ok = await compiler.init(callbacks, null);
  assert.ok(ok, `Could not initialize KmnCompiler (UnicodeSetParser), see callback messages`);
  if (ok) {
    return compiler;
  } else {
    return null;
  }
}

/** compare actual and expected strings */
export function assertCodePoints(actual?: string, expected?: string, msg?: string) {
  assert.strictEqual(actual, expected, `Actual ${msg||':'}\n${hex_str(actual)}\nExpected:\n${hex_str(expected)}\n`);
}

const dontEscape = /[a-zA-Z0-9\.${}\[\]-]/;

export function hex_str(s?: string) : string {
  return [...s].map(ch => dontEscape.test(ch) ? ch : util.escapeRegexChar(ch)).join('');
}

