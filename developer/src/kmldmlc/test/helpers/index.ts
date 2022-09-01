/**
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'path';
import * as fs from 'fs';
import { SectionCompiler } from '../../src/keyman/compiler/section-compiler';
import { Section } from '../../src/keyman/kmx/kmx-plus';
import LDMLKeyboardXMLSourceFileReader from '../../src/keyman/ldml-keyboard/ldml-keyboard-xml-reader';

/**
 * Builds a path to the fixture with the given path components.
 *
 * e.g., makePathToFixture('basic.xml')
 *
 * @param components One or more path components.
 */
export function makePathToFixture(...components: string[]): string {
  return path.join(__dirname, '..', '..', '..', 'test', 'fixtures', ...components);
}


export class CompilerCallbacks {
  messages: { code: number, message: string}[] = [];
  loadFile(baseFilename: string, filename:string): Buffer {
    // TODO: translate filename based on the baseFilename
    return fs.readFileSync(filename);
  }
  reportMessage(code: number, message: string): void {
    this.messages.push({code, message});
  }
}

export function loadSectionFixture(compilerClass: typeof SectionCompiler, filename: string, callbacks: CompilerCallbacks): Section {
  const inputFilename = makePathToFixture(filename);
  const source = (new LDMLKeyboardXMLSourceFileReader(callbacks)).loadFile(inputFilename);
  const compiler = new compilerClass(source, callbacks);
  if(!compiler.validate()) {
    return null;
  }
  return compiler.compile();
}
