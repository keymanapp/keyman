/**
 * Smoke-test the default
 */

import { assert } from 'chai';
import { default as breakWords } from '@keymanapp/models-wordbreakers';

const SHY = '\u00AD'; // Other, Format.  The "Soft HYphen" - usually invisible unless needed for word-wrapping.

describe('The default word breaker', function () {
  describe('default configuration', function() {
    it('should break multilingual text', function () {
      let breaks = breakWords(
        `Добрый день! ᑕᐻ᙮ — after working on ka${SHY}wen${SHY}non:${SHY}nis,
         let's eat phở! 🥣`
      );
      let words = breaks.map(span => span.text);
      assert.deepEqual(words, [
        'Добрый', 'день', '!', 'ᑕᐻ', '᙮', '—', 'after',
        'working', 'on', `ka${SHY}wen${SHY}non:${SHY}nis`, ',',
        "let's", 'eat', 'phở', '!', '🥣'
      ]);
    });

    it('handles heavily-punctuated English text', function() {
      // This test case brought to you by http://unicode.org/reports/tr29/#Word_Boundaries, Figure 1.
      let breaks = breakWords(
        `The quick ("brown") fox can't jump 32.3 feet, right?`
      );
      let words = breaks.map(span => span.text);
      assert.deepEqual(words, [
        'The', 'quick', '(', '"', 'brown', '"', ')', 'fox', "can't",
        'jump', '32.3', 'feet', ',', 'right', '?'
      ]);
    });

    // The way these two tests are written is a bit much on the "white-box" style,
    // but they do decently cover the boundary rules mentioned.
    it('Does not split empty contexts (WB1 + WB2)', function() {
      let breaks = breakWords('');
      let words = breaks.map(span => span.text);
      assert.deepEqual(words, []);
    });

    it('Does split at context boundaries (WB1 + WB2)', function() {
      let breaks = breakWords('a');
      let words = breaks.map(span => span.text);
      assert.deepEqual(words, ['a']);
    });

    // WB3, WB3a, WB3b are all handled internally, within the top-level function.

    // iff, as in "if and only if"
    it('ignores the zero-width joiner iff appropriate (WB4)', function() {
      const zwj = '\u200d';

      let breaks = breakWords(`a${zwj}b\n${zwj}c${zwj}\nd`);
      let words = breaks.map(span => span.text);

      // Does NOT ignore the zwj immediately after a newline - the notable exception
      // (the reason for "iff", not "if").
      assert.deepEqual(words, [`a${zwj}b`, `${zwj}`, `c${zwj}`, `d`]);
    })

    it('ignores extend characters iff appropriate (WB4)', function() {
      const comboGrave = '\u0300'; // The 'combining grave accent', as used in NFD.

      let breaks = breakWords(`a${comboGrave}e\n${comboGrave}i${comboGrave}\no`);
      let words = breaks.map(span => span.text);

      // Does NOT ignore the zwj immediately after a newline - the notable exception
      // (the reason for "iff", not "if").
      assert.deepEqual(words, [`a${comboGrave}e`, `${comboGrave}`, `i${comboGrave}`, `o`]);
    });

    it('ignores format characters iff appropriate (WB4)', function() {
      // Re-uses `const SHY` from above.
      let breaks = breakWords(`a${SHY}e\n${SHY}i${SHY}\no`);
      let words = breaks.map(span => span.text);

      // Does NOT ignore the zwj immediately after a newline - the notable exception
      // (the reason for "iff", not "if").
      assert.deepEqual(words, [`a${SHY}e`, `${SHY}`, `i${SHY}`, `o`]);
    });

    it('does not break between most alphabetic characters (WB5)', function() {
      let breaks = breakWords(`aəσאБ лאʈγX`); // a mix of latin, hebrew, greek, cyrillic, and IPA chars
                                              // for both "words".
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, [`aəσאБ`, `лאʈγX`]);
    });

    it('does not break letters across specific punctuation patterns (WB6, WB7)', function() {
      // `'`:  MidNumLetQ (from Single_Quote)
      // '.':  MidNumLet
      // ':':  MidLetter
      let breaks = breakWords(`don't b.r.e.a.k t:h:e:s:e`);
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, [`don't`, `b.r.e.a.k`, `t:h:e:s:e`]);

      let breaks2 = breakWords(`.drop: :the' 'extras.`);
      let words2 = breaks2.map(span => span.text);

      assert.deepEqual(words2, [`.`, `drop`, `:`, `:`, `the`, `'`, `'`, `extras`, `.`]);

      // ',': MidNum (is NOT included by rule!)
      let breaks3 = breakWords('do br,eak that');
      let words3 = breaks3.map(span => span.text);

      assert.deepEqual(words3, ['do', 'br', ',', 'eak', 'that']);
    });

    it('treats Hebrew properly (WB7a-c)', function() {
      const aleph = 'א';
      const bet = 'ב';

      // As Hebrew is RTL... this is probably the clearest way for us LTR people to
      // clearly see what's going on without ordering mechanics messing up the render.
      let breaks = breakWords(`${aleph}' ${aleph}" ${aleph}"${bet}`);
      let words = breaks.map(span => span.text);

      // A lingering double-quote isn't cool, but one in the middle's fine.
      // Lingering single-quote is fine regardless.
      assert.deepEqual(words, [`${aleph}'`, `${aleph}`, `"`, `${aleph}"${bet}`]);
    });

    it(`doesn't break within digit + digit/letter sequences (WB8-10)`, function() {
      let breaks = breakWords('a1b2c3 hunter2 ab12cd34 1234567890');
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, ['a1b2c3', 'hunter2', 'ab12cd34', '1234567890']);
    });

    it('does not break within formatted number sequences (WB11-12)', function() {
      // Note:  `'` fits "MidNumLetQ", part of the two rules!
      let breaks = breakWords(`1.2.3 3,458.01 3.45'8,01`);
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, [`1.2.3`, `3,458.01`, `3.45'8,01`]);

      let breaks2 = breakWords(`.1' ,3.`);
      let words2 = breaks2.map(span => span.text);

      assert.deepEqual(words2, [`.`, `1`, `'`, `,`, `3`, `.`]);
    });

    it('does not break between Katakana (WB13)', function() {
      const kataSmA = '\u30a2'; //ァ
      const kataA = '\u30a2'; //ア
      const kataSound = '\u309b'; // ゛

      let breaks = breakWords(`${kataSound}${kataA} ${kataSmA}${kataSound}b ${kataA}${kataSound}${kataSmA}`);
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, [
        `${kataSound}${kataA}`,
        `${kataSmA}${kataSound}`,
        'b',
        `${kataA}${kataSound}${kataSmA}`
      ]);
    });

    it('does not break form extenders (WB13a-b)', function() {
      // The `_` (underscore) fits the ExtendNumLet class this rule focuses on.
      const kataA = '\u30a2'; //ア

      let breaks = breakWords(`${kataA}_a__0_b_${kataA} _${kataA} 1_ _c_ ____`);
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, [
        `${kataA}_a__0_b_${kataA}`,
        `_${kataA}`,
        `1_`,
        `_c_`,
        `____`
      ]);
    });

    it('handles emoji flag sequences properly (WB15-16)', function() {
      // For clarity on what's being tested...
      let CA_FLAG =      '\u{1f1e8}\u{1f1e6}' // '🇨🇦' (canadian flag emoji); should not be broken.
      let KH_FLAG =      '\u{1f1f0}\u{1f1ed}' // '🇰🇭' (khmer flag emoji); same
      let X_FLAG_PIECE = '\u{1f1fd}'          // '🇽'  (half of a flag emoji; '🇽🇽' doesn't match a flag)
      let breaks = breakWords(`${CA_FLAG}${KH_FLAG}${X_FLAG_PIECE}${X_FLAG_PIECE}`);
      let words = breaks.map(span => span.text);

      // Note that the emoji may not render well within VSCode, but they show up nicely on GitHub.
      assert.deepEqual(words, ['🇨🇦', '🇰🇭', '🇽🇽']);
    });

    it('breaks hyphenated words by default', function() {
      let breaks = breakWords('Smith-Jones');
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, ['Smith', '-', 'Jones']);
    });
  });

  describe('customization', function() {
    // Refer to https://unicode.org/reports/tr29/#Word_Boundary_Rules, third bullet point.
    it('custom prop, rule:  do not break on letter-adjacent hyphens', function() {
      let customization = {
        rules: [{
          match: (context) => {
            if(context.propertyMatch(null, ["ALetter"], ["Hyphen"], ["ALetter"])) {
              return true;
            } else if(context.propertyMatch(["ALetter"], ["Hyphen"], ["ALetter"], null)) {
              return true;
            } else {
              return false;
            }
          },
          breakIfMatch: false
        }],
        propertyMapping: (char) => {
          const validHyphenCodes = [
            '\u002d', '\u2010', '\u058a', '\u30a0'
          ];
          if(validHyphenCodes.includes(char)) {
            return "Hyphen";
          }

          return null;
        },
        customProperties: ["Hyphen"]
      }

      let breaks = breakWords('Smith-Jones', customization);
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, ['Smith-Jones']);
    });

    it('mid-word hyphen via reassignment to MidLetter', function() {
      let customization = {
        propertyMapping: (char) => {
          const validHyphenCodes = [
            '\u002d', '\u2010', '\u058a', '\u30a0'
          ];
          if(validHyphenCodes.includes(char)) {
            return "MidLetter";
          }

          return null;
        }
      }

      let breaks = breakWords('Smith-Jones', customization);
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, ['Smith-Jones']);
    });

    // Useful for some regional minority languages that prefer word-breaking spaces.
    it('character reassignment:  Khmer letters as ALetter', function() {
      let customization = {
        propertyMapping: (char) => {
          if(char >= '\u1780' && char <= '\u17b3') {
            return "ALetter";
          } else {
            // The other Khmer characters already have useful word-breaking
            // property assignments.
            return null;
          }
        }
      }

      let breaks = breakWords('ស្រុក ខ្មែរ', customization);
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, ['ស្រុក', 'ខ្មែរ']);
    });

    // See:  suggested language-specific WB5a from the spec's notes.
    it("french/italian apostrophe / vowel boundaries", function() {
      let customization = {
        rules: [
          // WB5, but with differentiated consonants (ALetter) and vowels (AVowel)
          {
            match: (context) => {
              if(context.propertyMatch(null, ["ALetter", "AVowel"], ["ALetter", "AVowel"], null)) {
                return true;
              } else {
                return false;
              }
            },
            breakIfMatch: false
          },
          // Proposed WB5a
          {
            match: (context) => {
              if(context.propertyMatch(null, ["Single_Quote"], ["AVowel"], null)) {
                return true;
              } else {
                return false;
              }
            },
            breakIfMatch: true
          },
          // WB6, 7
          {
            match: (context) => {
              if(context.propertyMatch(null,
                                       ["ALetter", "AVowel"],
                                       ["MidLetter", "MidNumLet", "Single_Quote"],
                                       ["ALetter", "AVowel"])) {
                return true;
              } else if(context.propertyMatch(["ALetter", "AVowel"],
                                              ["MidLetter", "MidNumLet", "Single_Quote"],
                                              ["ALetter", "AVowel"],
                                              null)) {
                return true;
              } else {
                return false;
              }
            },
            breakIfMatch: false
          }
          // Similar extensions to WB9, 10, 13a, and 13b would also be needed for robustness.
          // And I kind of left the Hebrew_Letter out of the WB5, 6, and 7 rewrites.
        ],
        propertyMapping: (char) => {
          const vowels = ['a', 'e', 'i', 'o', 'u'];
          if(vowels.includes(char)) {
            return "AVowel";
          }

          return null;
        },
        customProperties: ["AVowel"]
      }

      let breaks = breakWords("l'objectif aujourd'hui", customization);
      let words = breaks.map(span => span.text);

      assert.deepEqual(words, ["l'", "objectif", "aujourd'hui"]);
    });
  });
});
