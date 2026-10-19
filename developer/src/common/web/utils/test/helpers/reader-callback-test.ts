/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Various helpers for LDML XML reader unit tests
 */
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';
import 'mocha';
import {assert} from 'chai';

import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { loadFile, makePathToFixture } from './index.js';

import { LDMLKeyboardXMLSourceFileReader, LDMLKeyboardXMLSourceFileReaderOptions } from '../../src/types/ldml-keyboard/ldml-keyboard-xml-reader.js';
import { CompilerError, CompilerErrorSeverity, CompilerEvent } from '../../src/compiler-interfaces.js';
import { LDMLKeyboardXMLSourceFile } from '../../src/types/ldml-keyboard/ldml-keyboard-xml.js';
import { LDMLKeyboardTestDataXMLSourceFile } from '../../src/types/ldml-keyboard/ldml-keyboard-testdata-xml.js';

const readerOptions: LDMLKeyboardXMLSourceFileReaderOptions = {
  cldrImportsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL)),
  localImportsPaths: [],
};

export interface CompilationCase {
  /**
   * If true, loading (validation) should fail
   */
  loadfail?: boolean;
  /**
   * path to xml, such as 'sections/layr/invalid-case.xml'
   */
  subpath: string;
  /**
   * message codes. If present, must be a match for all messages reported; use
   * to avoid complicating unit tests with the actual text of messages and
   * message parameters (we don't normally want to test message content)
   */
  messageCodes?: number[];
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
  callback?: (data: Buffer, source: LDMLKeyboardXMLSourceFile, subpath: string, callbacks: TestCompilerCallbacks ) => void;
  /**
   * if present, expect compiler to throw (use .* to match all)
   */
  throws?: RegExp;
}


export interface TestDataCase {
  /**
   * If true, loading (validation) should fail
   */
  loadfail?: boolean;
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
  callback?: (data: Buffer, source: LDMLKeyboardTestDataXMLSourceFile, subpath: string, callbacks: TestCompilerCallbacks ) => void;
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
 *
 * TODO(lowpri): split .load and .validate test cases so we are properly unit testing
 */
export function testReaderCases(cases : CompilationCase[]) {
  // we need our own callbacks rather than using the global so messages don't get mixed
  const callbacks = new TestCompilerCallbacks();
  for (const testcase of cases) {
    const expectFailure =  // if true, we expect this to fail
      testcase.throws ||
      !!(testcase.errors) ||  // expect failure if test case contains any error messages
      !!(testcase.messageCodes?.filter(code => CompilerError.severity(code) >= CompilerErrorSeverity.Error)?.length); // only mark as failed if we have error or fatal messages
    const testHeading = expectFailure ? `should fail to load: ${testcase.subpath}`:
                                        `should load: ${testcase.subpath}`;
    it(testHeading, function () {
      callbacks.clear();
      const path = makePathToFixture('ldml-keyboard', testcase.subpath);
      // update readerOptions to point to the source dir.
      readerOptions.localImportsPaths = [ dirname(path) ];
      const reader = new LDMLKeyboardXMLSourceFileReader(readerOptions, callbacks);
      const data = loadFile(path);
      assert.ok(data, `reading ${testcase.subpath}`);
      const source = reader.load(data);
      if (!testcase.loadfail) {
        if (!source) {
          // print any loading errs here
          if (testcase.warnings) {
            assert.includeDeepMembers(callbacks.messages, testcase.warnings, 'expected warnings to be included');
          } else if (!expectFailure) {
            // no warnings, so expect zero messages
            assert.deepEqual(callbacks.messages, [], 'expected zero messages');
          }
        }
        assert.ok(source, `loading ${testcase.subpath}`);
      } else {
        assert.notOk(source, `loading ${testcase.subpath} (expected failure)`);
      }
      // special case for an expected exception
      if (testcase.throws) {
        assert.throws(() => reader.validate(source), testcase.throws);
      } else {
        assert.doesNotThrow(() => reader.validate(source), `validating ${testcase.subpath}`);
        if (testcase.messageCodes) {
          assert.sameDeepMembers(callbacks.messages.map(message => message.code), testcase.messageCodes, 'expected messages to match');
        } else {
          // if we expected errors or warnings, show them
          if (testcase.errors) {
            assert.includeDeepMembers(callbacks.messages, testcase.errors, 'expected errors to be included');
          }
          if (testcase.warnings) {
            assert.includeDeepMembers(callbacks.messages, testcase.warnings, 'expected warnings to be included');
          } else if (!expectFailure) {
            // no warnings, so expect zero messages
            assert.deepEqual(callbacks.messages, [], 'expected zero messages');
          }
        }

        // run the user-supplied callback if any
        if (testcase.callback) {
          testcase.callback(data, source, testcase.subpath, callbacks);
        }
      }
    });
  }
}


/**
 * Run a bunch of cases
 * @param cases cases to run
 * @param compiler argument to loadSectionFixture()
 * @param callbacks argument to loadSectionFixture()
 */
export function testTestdataReaderCases(cases : TestDataCase[]) {
  // we need our own callbacks rather than using the global so messages don't get mixed
  const callbacks = new TestCompilerCallbacks();
  const reader = new LDMLKeyboardXMLSourceFileReader(readerOptions, callbacks);
  for (const testcase of cases) {
    const expectFailure = testcase.throws || !!(testcase.errors); // if true, we expect this to fail
    const testHeading = expectFailure ? `should fail to load: ${testcase.subpath}`:
                                        `should load: ${testcase.subpath}`;
    it(testHeading, function () {
      callbacks.clear();

      const data = loadFile(makePathToFixture('ldml-keyboard', testcase.subpath));
      assert.ok(data, `reading ${testcase.subpath}`);
      const source = reader.loadTestData(data);
      if (!testcase.loadfail) {
        assert.ok(source, `loading ${testcase.subpath}`);
      } else {
        assert.notOk(source, `loading ${testcase.subpath} (expected failure)`);
      }
      // special case for an expected exception
      // TODO-LDML: no validation for now.
      // if we expected errors or warnings, show them
      if (testcase.errors) {
        assert.includeDeepMembers(callbacks.messages, testcase.errors, 'expected errors to be included');
      }
      if (testcase.warnings) {
        assert.includeDeepMembers(callbacks.messages, testcase.warnings, 'expected warnings to be included');
      } else if (!expectFailure) {
        // no warnings, so expect zero messages
        assert.strictEqual(callbacks.messages.length, 0, 'expected zero messages but got ' +callbacks.messages.map(e => e.message).join(' ') );
      }

      // run the user-supplied callback if any
      if (testcase.callback) {
        testcase.callback(data, source, testcase.subpath, callbacks);
      }
    });
  }
}
