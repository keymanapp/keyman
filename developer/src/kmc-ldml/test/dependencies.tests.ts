import 'mocha';
import {assert} from 'chai';
import { SECTION_COMPILERS } from '../src/compiler/compiler.js';
import { SectionIdent } from '@keymanapp/ldml-keyboard-constants';
describe('test of section compiler dependencies', () => {
  it('should have dependencies in the right order', () => {
    const sects : Set<SectionIdent> = new Set();
    for (const sect of SECTION_COMPILERS) {
      // construct the compiler
      const c = new sect({ keyboard3: null }, null); // For now, this is OK for the inspection
      const id = c.id;
      assert.ok(id);
      assert.isFalse(sects.has(id), `Duplicate compiler ${id} in SECTION_COMPILERS`);
      assert.ok(c.dependencies);
      // make sure all deps are present
      for (const dep of c.dependencies.values()) {
        assert.ok(dep);
        assert.isTrue(sects.has(dep), `Section ${id} requested uninitialized dependency ${dep}: check SECTION_COMPILERS order`);
      }
      // add this to the list
      sects.add(id);
    }
  });
});

