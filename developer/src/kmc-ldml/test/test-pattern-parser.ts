import 'mocha';
import { assert } from 'chai';
import { MarkerParser } from '../src/util/pattern-parser.js';

describe('Test of Pattern Parsers', () => {
  describe('should test MarkerParser', () => {
    it('should accept matching ids', () => {
      for (const id of [
        'm1',
        'dead_key',
        'alif',
        'Alif',
        'Alif2',
      ]) {
        assert.ok(MarkerParser.ID.test(id), `expected ok: ${id}`);
      }
    });
    it('should reject non matching ids', () => {
      for (const id of [
        '',
        'Some Thing',
        'ναι',
        'SUPERCALIFRAGILISTICEXPIALIDOCIOUS',
        '.', // reserved
      ]) {
        assert.notOk(MarkerParser.ID.test(id), `expected false: ${id}`);
      }
    });
    // indirectly tests REFERENCE
    it('should match reference strings', () => {
      const cases: string[][] = [
        ['\\m{acute}', 'acute'],
        ['\\m{acute}≈\\m{acute}', 'acute acute'], // not deduped
        ['\\m{grave}≠\\m{acute}', 'grave acute'],
        [MarkerParser.ANY_MARKER, MarkerParser.ANY_MARKER_ID],
        ['[a-z]\\m{.}', MarkerParser.ANY_MARKER_ID],
      ];
      for (const [str, refs] of cases) {
        const reflist = refs.split(' ').sort();
        assert.sameDeepMembers(MarkerParser.allReferences(str), reflist, `for ${str}`);
      }
    });
    it('non matching reference strings', () => {
      for (const str of [
        '',
        'Some Thing',
        'ναι',
        '\m{issed opportunity',
        // 'This is: \\m{escaped}', // need a backreference
      ]) {
        assert.deepEqual(MarkerParser.allReferences(str), [], `expected no markers: ${str}`);
      }
    });
  });
});
