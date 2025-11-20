/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';
import 'mocha';
import { assert } from 'chai';
import { compilerEventFormat, CompilerOptions, } from "@keymanapp/developer-utils";
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';


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

export const compilerTestOptions: CompilerOptions = {};

beforeEach(function() {
  compilerTestCallbacks.clear();
});

afterEach(function() {
  if (this.currentTest.state !== 'passed') {
    compilerTestCallbacks.messages.forEach(message => console.log(message.message));
  }
});

export function checkMessages() {
  if(compilerTestCallbacks.messages.length > 0) {
    console.log(compilerTestCallbacks.messages);
  }
  assert.isEmpty(compilerTestCallbacks.messages, compilerEventFormat(compilerTestCallbacks.messages));
}
