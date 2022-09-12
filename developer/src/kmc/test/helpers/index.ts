/**
 * Helpers and utilities for the Mocha tests.
 */
import * as path from 'path';
import * as fs from 'fs';
import { SectionCompiler } from '../../src/keyman/compiler/section-compiler';
import { Elem, GlobalSections, Section, Strs } from '../../src/keyman/kmx/kmx-plus';
import LDMLKeyboardXMLSourceFileReader from '../../src/keyman/ldml-keyboard/ldml-keyboard-xml-reader';
import { CompilerEvent } from '../keyman/compiler/callbacks';

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
  messages: CompilerEvent[] = [];
  loadFile(baseFilename: string, filename:string): Buffer {
    // TODO: translate filename based on the baseFilename
    return fs.readFileSync(filename);
  }
  reportMessage(event: CompilerEvent): void {
    this.messages.push(event);
  }
  loadLdmlKeyboardSchema(): Buffer {
    return fs.readFileSync(path.join(__dirname, '..', '..', 'src', 'ldml-keyboard.schema.json'));
  }
}

export function loadSectionFixture(compilerClass: typeof SectionCompiler, filename: string, callbacks: CompilerCallbacks): Section {
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
