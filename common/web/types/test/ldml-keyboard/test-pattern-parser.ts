import 'mocha';
import { assert } from 'chai';
import { ElementParser, ElementSegment, ElementType, MarkerParser, OrderedStringList, VariableParser } from '../../src/ldml-keyboard/pattern-parser.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { KMX } from '../../src/main.js';

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
    it('should be able to emit sentinel values', () => {
      assert.equal(MarkerParser.markerOutput(295), '\uFFFF\u0008\u0127', 'Wrong sentinel value emitted');
      assert.equal(MarkerParser.markerOutput(MarkerParser.ANY_MARKER_INDEX), '\uFFFF\u0008\uFFFE', 'Wrong sentinel value emitted for ffff');
      assert.throws(() => MarkerParser.markerOutput(0)); // below MIN
      assert.throws(() => MarkerParser.markerOutput(0x10000)); // above MAX
    });
    it('should be able to output sentinel strings', () => {
      // with nothing (no markers)
      assert.equal(
        MarkerParser.toSentinelString(`No markers here!`),
        `No markers here!`
      );
      assert.throws(() =>
        MarkerParser.toSentinelString(`Marker \\m{sorryNoMarkers}`)
      );
      // with a custom class
      class MyMarkers implements OrderedStringList {
        getItemOrder(item: string): number {
          const m : any = {
            'a': 0,
            'b': 1,
            'c': 2,
            'zzz': 0x2FFFFF,
          };
          const o = m[item];
          if (o === undefined) return -1;
          return o;
        }
      };
      const markers = new MyMarkers();
      assert.equal(MarkerParser.toSentinelString(
        `No markers here!`, markers),
        `No markers here!`
      );
      assert.equal(MarkerParser.toSentinelString(
        `Give me \\m{a} and \\m{c}, or \\m{.}.`, markers),
        `Give me \uFFFF\u0008\u0001 and \uFFFF\u0008\u0003, or \uFFFF\u0008\uFFFE.`
      );
      assert.throws(() =>
        MarkerParser.toSentinelString(
          `Want to see something funny? \\m{zzz}`, // out of range
          markers
        )
      );
      assert.throws(() =>
        MarkerParser.toSentinelString(
          `Want to see something sad? \\m{nothing}`, // non existent
          markers
        )
      );
    });
    it('should match some marker constants', () => {
      assert.equal(constants.marker_sentinel, KMX.KMXFile.UC_SENTINEL);
      assert.equal(constants.marker_code_deadkey, KMX.KMXFile.CODE_DEADKEY);
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
      it('should be able to run some splitters', () => {
        assert.sameDeepMembers(VariableParser.allStringReferences(
          ``
        ), [
        ], `running allStringReferences('')`);

        assert.sameDeepMembers(VariableParser.allStringReferences(
          '${str1} ${str2}'
        ), [
          'str1', 'str2',
        ], `running allStringReferences('\${str1} \${str2}')`);

        assert.sameDeepMembers(VariableParser.allSetReferences(
          '',
        ), [
        ], `running allSetReferences('')`);
        assert.sameDeepMembers(VariableParser.allSetReferences(
          ' $[set1] $[set2] ',
        ), [
          'set1', 'set2',
        ], `running allSetReferences(' \$[set1] \$[set2]')`);
        assert.sameDeepMembers(VariableParser.setSplitter(
          ``
        ), [
        ], `running setSplitter('')`);
        assert.sameDeepMembers(VariableParser.setSplitter(
          ` A B  C`
        ), [
          'A', 'B', 'C',
        ], `running setSplitter(' A B  C')`);
      });
    });
    describe('segment some strings', () => {
      it('should have a functioning ElementSegment() c’tor', () => {
        assert.equal(new ElementSegment('String', ElementType.string).type, ElementType.string);
        assert.equal(new ElementSegment('String', ElementType.string).unescaped, 'String');
        assert.equal(new ElementSegment('\\u0041').unescaped, 'A');
        assert.equal(new ElementSegment('\\u{0041}').unescaped, 'A');
      });
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
            type: ElementType.escaped,
          },{
            segment: '[\\u1A75-\\u1A79]',
            type: ElementType.uset,
          },{
            segment: '\\u1A45',
            type: ElementType.escaped,
          },{
            segment: 'ħ',
            type: ElementType.codepoint,
          }],
        },
        {
          str: `\\u{22}\\u{0127}`,
          expect: [{
            segment: `\\u{22}`,
            type: ElementType.escaped,
          },
          {
            segment: `\\u{0127}`,
            type: ElementType.escaped,
          }],
        },
        {
          str: `\\u{22 0127}`,
          expect: [{
            segment: `\\u{22}`,
            type: ElementType.escaped,
          },
          {
            segment: `\\u{127}`, // resegmented with minimal hex
            type: ElementType.escaped,
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
