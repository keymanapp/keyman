import 'mocha';
import { assert } from 'chai';
import { ElementParser, ElementSegment, ElementType, MARKER_BEFORE_EOT, MarkerMap, MarkerParser, MarkerResult, OrderedStringList, VariableParser } from '../../src/ldml-keyboard/pattern-parser.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { KMXFile } from '../../src/kmx/kmx.js';
import Hexy from 'hexy';
const { hexy } = Hexy;

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
        ['\\m{acute} but not \\\\m{chronic}', 'acute'], // second marker is escaped
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
    it('should match broken reference strings', () => {
      const cases: string[][] = [
        // hyphenated marker id - illegal
        ['\\m{chronic} \\m{a-cute} \\\\m{a-choo}', 'a-cute'], // \\m{a-choo} is literal
        // marker through end of line
        ['\\m{chronic} \\m{oopsIforGot to terminate it', 'oopsIforGot to terminate it'],
        // marker terminated by other valid marker
        ['\\m{chronic} \\m{what \\m{does} \\m{this button do?', 'what  ', 'this button do?'],
      ];
      for (const [str, ...reflist] of cases) {
        assert.sameDeepMembers(MarkerParser.allBrokenReferences(str), reflist, `for ${str}`);
      }
    });
    it('should be able to emit sentinel values', () => {
      assert.equal(MarkerParser.markerOutput(295), '\uFFFF\u0008\u0127', 'Wrong sentinel value emitted');
      assert.equal(MarkerParser.markerOutput(MarkerParser.ANY_MARKER_INDEX), '\uFFFF\u0008\uD7FF', 'Wrong sentinel value emitted for ANY_MARKER_INDEX');
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
            'zz': MarkerParser.MAX_MARKER_INDEX - 1, // this is an ordering, so needs to be -1
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
        `Give me \uFFFF\u0008\u0001 and \uFFFF\u0008\u0003, or \uFFFF\u0008\uD7FF.`
      );
      assert.equal(MarkerParser.toSentinelString(
        `Give me \\m{a} and \\m{c}, or \\m{.}.`, markers, true),
        `Give me \\uffff\\u0008\\u0001 and \\uffff\\u0008\\u0003, or ${MarkerParser.ANY_MARKER_MATCH}.`
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
      // verify the matching behavior of these
      assert.isTrue(new RegExp(MarkerParser.toSentinelString(`^Q\\m{a}$`, markers, true), 'u')
        .test(MarkerParser.toSentinelString(`Q\\m{a}`, markers, false)), `Q\\m{a} did not match`);
      assert.isFalse(new RegExp(MarkerParser.toSentinelString(`^Q\\m{a}$`, markers, true), 'u')
        .test(MarkerParser.toSentinelString(`Q\\m{b}`, markers, false)), `Q\\m{a} should not match Q\\m{b}`);
      assert.isTrue(new RegExp(MarkerParser.toSentinelString(`^Q\\m{.}$`, markers, true), 'u')
        .test(MarkerParser.toSentinelString(`Q\\m{a}`, markers, false)), `Q\\m{.} did not match Q\\m{a}`);
      assert.isTrue(new RegExp(MarkerParser.toSentinelString(`^Q\\m{.}$`, markers, true), 'u')
        .test(MarkerParser.toSentinelString(`Q\\m{zz}`, markers, false)), `Q\\m{.} did not match Q\\m{zz} (max marker)`);
      assert.isFalse(new RegExp(MarkerParser.toSentinelString(`^Q\\m{.}$`, markers, true), 'u')
        .test(MarkerParser.toSentinelString(`\\m{a}`, markers, false)), `Q\\m{.} did not match \\m{a}`);
      assert.isTrue(new RegExp(MarkerParser.toSentinelString(`^\\m{.}$`, markers, true), 'u')
        .test(MarkerParser.toSentinelString(`\\m{a}`, markers, false)), `\\m{.} did not match \\m{a}`);
      assert.isFalse(new RegExp(MarkerParser.toSentinelString(`^\\m{.}$`, markers, true), 'u')
        .test(MarkerParser.toSentinelString(`\\m{a}\\m{b}`, markers, false)), `\\m{.} did not match \\m{a}\\m{b}`);
    });
    it('should match some marker constants', () => {
      assert.equal(constants.uc_sentinel, KMXFile.UC_SENTINEL);
      assert.equal(constants.marker_code, KMXFile.CODE_DEADKEY);
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


describe('Test of nfd_markers()', () => {
  it('should be able to parse_next_markers()', () => {
    [
      ["6e", null, false],
      ["6e", null, true],
      ["\uffff\u0008\u0001e", { match: "\uffff\u0008\u0001", marker: 1 }, false],
      [MarkerParser.ANY_MARKER_MATCH, {
        match: MarkerParser.ANY_MARKER_MATCH,
        marker: constants.marker_any_index,
      }, true],
      [
        "\\uffff\\u0008\\u0002abc", { match: "\\uffff\\u0008\\u0002", marker: 2 }, true
      ],
    ].forEach(([src,expect,forMatch]) => {
      const dst = MarkerParser.parse_next_marker(<string>src, <boolean>forMatch);
      assert.deepEqual(dst, <MarkerResult>expect, `Parsing ${src} with forMatch=${forMatch}`);
    });
  });
  it('should be able to remove_markers()', () => {
    [
      ["6e","6e",false],
      ["6e","6e",true],
      ["6\uffff\u0008\u0001e", "6e", false],
      ["6\\uffff\\u0008\\u0001e", "6e", true],
      [`6${MarkerParser.ANY_MARKER_MATCH}e`, "6e", true],
    ].forEach(([src,expect,forMatch]) => {
      const dst = MarkerParser.remove_markers(<string>src, [], <boolean>forMatch);
      assert.equal(dst, <string>expect, `mapping ${src} with forMatch=${forMatch}`);
    });
  });
  it('should be able to remove_markers() with composed chars', () => {
    const src = '\uffff\u0008\u0001\u0344';
    const m : MarkerMap = [];
    const dst = MarkerParser.remove_markers(src, m, false);
    assert.equal(dst, '\u0344'); // nfc
    assert.sameDeepOrderedMembers(m, [
      { ch: '\u0308', end: true },
      { ch: '\u0308', marker: 1},
      { ch: '\u0301', end: true},
      { ch: MARKER_BEFORE_EOT, end: true },
    ]);
  });
  it('should normalize as expected', () => {
    // this is a little bit simpler in structure than what's in test_transforms.cpp,
    // see there for more complicated cases and discussion
    const src_expect = [
      // ["src", "expect", "regex"]
      // or ["src"] if it is expected to be unchanged

      // #1
      ["abc"],
      ["6\uffff\u0008"],
      ["6\uffffq"],
      ["6\uffff"],
      ["6\uffffzz"],
      ["6\uffff\u0008\u0001"],
      ["6e\u0320\u0300"],
      // #8
      ["6e\u0300\u0320","6e\u0320\u0300"],
      ["6\uffff\u0008\u0001e\uffff\u0008\u0002\u0320\uffff\u0008\u0003\u0300\uffff\u0008\u0004"],
      ["6\uffff\u0008\u0001e\uffff\u0008\u0002\u0320\uffff\u0008\u0003\u0300\uffff\u0008\u0004"],
      // out of order
      ["6\uffff\u0008\u0001e\uffff\u0008\u0002\u0300\uffff\u0008\u0003\u0320\uffff\u0008\u0004",
       "6\uffff\u0008\u0001e\uffff\u0008\u0003\u0320\uffff\u0008\u0002\u0300\uffff\u0008\u0004"],
      ["4e\u0300\uFFFF\u0008\u0001\u0320",
       "4e\uFFFF\u0008\u0001\u0320\u0300"],
      ["9ce\u0300\uFFFF\u0008\u0002\u0320\uFFFF\u0008\u0001",
       "9ce\uFFFF\u0008\u0002\u0320\u0300\uFFFF\u0008\u0001"],
      ["9ce\u0300\\uffff\\u0008\\u0002\u0320\\uffff\\u0008\\u0001",
       "9ce\\uffff\\u0008\\u0002\u0320\u0300\\uffff\\u0008\\u0001",             "REGEX"],
      ["9ce\u0300\\uffff\\u0008[\\u0001-\\ud7fe]\u0320\\uffff\\u0008\\u0001",
       "9ce\\uffff\\u0008[\\u0001-\\ud7fe]\u0320\u0300\\uffff\\u0008\\u0001",   "REGEX"],
      ["9ce\u0300\uFFFF\u0008\u0002\uFFFF\u0008\u0002\u0320",
      "9ce\uFFFF\u0008\u0002\uFFFF\u0008\u0002\u0320\u0300"],
      ["9ce\u0300\uFFFF\u0008\u0002\uFFFF\u0008\u0001\uFFFF\u0008\u0003\u0320",
      "9ce\uFFFF\u0008\u0002\uFFFF\u0008\u0001\uFFFF\u0008\u0003\u0320\u0300"],
      ["e\uFFFF\u0008\u0001\u0300\uFFFF\u0008\u0002\u0320E\uFFFF\u0008\u0003\u0300\uFFFF\u0008\u0004\u0320",
       "e\uFFFF\u0008\u0002\u0320\uFFFF\u0008\u0001\u0300E\uFFFF\u0008\u0004\u0320\uFFFF\u0008\u0003\u0300"],
      // additional tests with denormalized glue chars
      ["\u0995\uFFFF\u0008\u0001\u09CB", "\u0995\uFFFF\u0008\u0001\u09C7\u09BE"],
      ["\u0995\u09BE\uFFFF\u0008\u0001\u09C7"],
      ["\u03B5\uFFFF\u0008\u0001\u0344", "\u03B5\uFFFF\u0008\u0001\u0308\u0301"],

      // double marker -
      ["e\uffff\u0008\u0001\u0300\u0320\u0300", "e\u0320\uffff\u0008\u0001\u0300\u0300"],
      // double marker - no change
      ["e\u0320\uffff\u0008\u0001\u0300\u0300", "e\u0320\uffff\u0008\u0001\u0300\u0300"],

      // Double marker with greek!
      ["\u03B5\uFFFF\u0008\u0001\u0344\uFFFF\u0008\u0002\u0344\uFFFF\u0008\u0003",
       "\u03B5\uFFFF\u0008\u0001\u0308\u0301\uFFFF\u0008\u0002\u0308\u0301\uFFFF\u0008\u0003"],

      // double marker -
      ["e\uffff\u0008\u0001\u0300\u0320\u0300", "e\u0320\uffff\u0008\u0001\u0300\u0300"],
      // double marker - no change
      ["e\u0320\uffff\u0008\u0001\u0300\u0300", "e\u0320\uffff\u0008\u0001\u0300\u0300"],
      // double marker - in front of second, no change
      ["e\u0320\u0300\uffff\u0008\u0001\u0300", "e\u0320\u0300\uffff\u0008\u0001\u0300"],
      // double marker - in front of second, with segment reordering
      ["e\u0300\u0320\uffff\u0008\u0001\u0300", "e\u0320\u0300\uffff\u0008\u0001\u0300"],
      // double marker - alternate pattern with reordering needed
      ["e\u0300\uffff\u0008\u0001\u0300\u0320", "e\u0320\u0300\uffff\u0008\u0001\u0300"],
      // triple diacritic + marker - reordering needed
      ["e\u0300\uffff\u0008\u0001\u0300\u0320\u0300", "e\u0320\u0300\uffff\u0008\u0001\u0300\u0300"],


    ];

    for (let i = 0; i < src_expect.length; i++) {
      const r = src_expect[i];
      const src = r[0];
      const exp = r[1] || src;
      let forMatch = false;
      if (r.length > 2) {
        // regex
        forMatch = true;
      }
      let dst;
      try {
        dst = MarkerParser.nfd_markers(src, forMatch);
      } catch(e) {
        console.error(e);
        assert.fail(`#${i+1} normalizing '${src}'`);
      }
      assert.ok(dst, `#${i+1} normalizing '${src}'`);
      assert.equal(dst, exp, `#${i+1} normalizing '${src}' forMatch=${forMatch} (got \n${hex_str(dst)} expected \n${hex_str(exp)}) `);
    }
  });
});

function hex_str(s?: string) : string {
  return hexy(Buffer.from(s, 'utf16le'), {});
}
