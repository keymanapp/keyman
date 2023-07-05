import 'mocha';
import { assert } from 'chai';
import { ElementParser, ElementType, MarkerParser, VariableParser } from '../../src/ldml-keyboard/pattern-parser.js';

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
  describe('should test VariableParser', () => {
    // same test as for markers
    it('should accept matching ids', () => {
      for (const id of [
        'm1',
        'dead_key',
        'alif',
        'Alif',
        'Alif2',
      ]) {
        assert.ok(VariableParser.ID.test(id), `expected ok: ${id}`);
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
        assert.notOk(VariableParser.ID.test(id), `expected false: ${id}`);
      }
    });
  });
  describe('ElementParser', () => {
    const samplePatterns = [
      `\\u1A60`,
      `[\\u1A75-\\u1A79]`,
      `\\u1A60\\u1A45`,
      `\\u1A60[\\u1A75-\\u1A79]\\u1A45`,
      `ែ្ម`,
    ]
    describe('try out the regexes', () => {
      it('should detect usets', () => {
        [
          `[a-z]`,
          `[[a-z]-[aeiou]]`,
        ].forEach(s => assert.ok(ElementParser.MATCH_USET.test(s), `expected true: ${s}`));
      });
      it('should detect non usets', () => {
        [
          `\\u0127`,
          `\\u{22}`,
          `x`,
        ].forEach(s => assert.notOk(ElementParser.MATCH_USET.test(s), `expected false: ${s}`));
      });
      it('should detect escaped', () => {
        [
          `\\u0127`,
          `\\u{22}`,
        ].forEach(s => assert.ok(ElementParser.MATCH_ESCAPED.test(s), `expected true: ${s}`));
      });
      it('should detect non escaped', () => {
        [
          `[a-z]`,
          `[[a-z]-[aeiou]]`,
          `ê`,
        ].forEach(s => assert.notOk(ElementParser.MATCH_ESCAPED.test(s), `expected false: ${s}`));
      });
      it('should reject nested square brackets', () => {
        [
          `[[a-z]-[aeiou]]`,
        ].forEach(s => assert.ok(ElementParser.MATCH_NESTED_SQUARE_BRACKETS.test(s), `expected true: ${s}`));
      });
      it('should allow non-nested square brackets', () => {
        [
          `[a-z]`,
          `ê`,
          ...samplePatterns,
        ].forEach(s => assert.notOk(ElementParser.MATCH_NESTED_SQUARE_BRACKETS.test(s), `expected false: ${s}`));
      });
    });
    describe('segment some strings', () => {
      it('should be able to segment strings from the spec and samples', () => {
        samplePatterns.forEach(str => assert.ok(ElementParser.segment(str)));
      });
      it('should throw on nested brackets', () => {
        [
          `[[a-z]-[aeiou]]`,
        ].forEach(str => assert.throws(() => ElementParser.segment(str)));
      });
      [
        {
          str: `ê🙀`,
          expect: [{
            segment: 'ê',
            type: ElementType.codepoint
          },{
            segment: '🙀',
            type: ElementType.codepoint
          }],
        },
        {
          str: `\\u1A60[\\u1A75-\\u1A79]\\u1A45ħ`,
          expect: [{
            segment: '\\u1A60',
            type: ElementType.escaped
          },{
            segment: '[\\u1A75-\\u1A79]',
            type: ElementType.uset
          },{
            segment: '\\u1A45',
            type: ElementType.escaped
          },{
            segment: 'ħ',
            type: ElementType.codepoint
          }],
        },
      ].forEach(({str, expect}) => it(`Segment: ${str}`, () => {
        const segmented = ElementParser.segment(str);
        assert.ok(segmented, `segmenting ${str}`);
        assert.deepEqual(segmented, expect, `segments of ${str}`)
      }));
    });
  });
});
