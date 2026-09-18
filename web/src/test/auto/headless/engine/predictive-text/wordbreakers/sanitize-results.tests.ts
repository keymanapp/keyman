/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-09-18
 *
 * This file defines tests for our backward-compatibility wrapper for custom
 * model wordbreakers that don't properly handle/report whitespace tokens and/or
 * don't robustly mark the start of their spans correctly.
 */

import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { defaultWordbreaker, LazySpan, sanitizeResults } from '@keymanapp/models-wordbreakers';

import Span = LexicalModelTypes.Span;
import WordBreakingFunction = LexicalModelTypes.WordBreakingFunction;

// Taken from `ait.mnw.mon` v1.1
const simpleNaiveCustomBreaker: WordBreakingFunction = (str: string) => {
  return str.split(/\s|\u200b/).map(function(token) {
    // Is actually missing the 'length' portion!
    return {
      left: str.indexOf(token),
      start: str.indexOf(token),
      right: str.indexOf(token) + token.length,
      end: str.indexOf(token) + token.length,
      text: token
    } as undefined as Span;
  });
};

// Taken from `sil.km.gcc` v2.0
const extendedCustomBreaker: WordBreakingFunction = (str) => {
  const whitespaceRegex = /\s|\u200b|\n|\r/;
  const tokens = str.split(whitespaceRegex);

  for(let i=0; i < tokens.length; i++) {
    const token = tokens[i];
    if(token.length == 0) {
      tokens.splice(i, 1);
      i--;
      continue;
    } else if(token.length == 1 && whitespaceRegex.test(token)) {
      tokens.splice(i, 1);
      i--;
      continue;
    }

    // Certain punctuation marks should be considered a separate token from the word they're next to.
    const punctuationMarks = ['«', '»', '$', '#' /* add extras here */];
    const punctSplitIndices = [];

    // Find if and where each mark exists within the token
    for(let i = 0; i < punctuationMarks.length; i++) {
      const split = token.indexOf(punctuationMarks[i]);
      if(split >= 0) {
        punctSplitIndices.push(split);
      }
    }

    // Sort and pick the earliest mark's location.  If none exists, use -1.
    punctSplitIndices.sort();
    const splitPoint = punctSplitIndices[0] === undefined ? -1 : punctSplitIndices[0];

    if(splitPoint > -1) {
      const left = token.substring(0, splitPoint);  // (0, -1) => ''
      const punct = token.substring(splitPoint, splitPoint+1);
      const right = token.substring(splitPoint+1);  // Starting past the end of the string => ''

      if(left) {
        tokens.splice(i++, 0, left);
      }
      tokens.splice(i++, 1, punct);
      if(right) {
        tokens.splice(i, 0, right);
      }
      // Ensure that the next iteration puts `i` immediately after the punctuation token... even if
      // there was a `right` portion, as it may have extra marks that also need to be spun off.
      i--;
    }
  }

  let latestIndex = 0;
  return tokens.map(function(token) {
    const start = str.indexOf(token, latestIndex);
    latestIndex = start + token.length;
    return {
      left: start,
      start: start,
      right: start + token.length,
      end: start + token.length,
      length: token.length,
      text: token
    }
  });
};

const u = (code: number) => String.fromCodePoint(code);

// Uses both BMP & non-BMP chars that essentially spell out "apple".
const mixedPlaneApple = 'a' + u(0x1d5c9) + 'p' + 'l' + u(0x1d5be);

describe('Custom wordbreaker whitespace restoration', function () {
  describe('wrapping the Unicode default wordbreaker', () => {
    const breaker = sanitizeResults(defaultWordbreaker);

    this.beforeAll(() => {
      assert.isOk(breaker);
    });

    it('properly breaks a context with two spaces', () => {
      const context = '  ';
      const spans = breaker(context);
      assert.deepEqual(spans.map((s) => new LazySpan(context, s.start, s.end)), defaultWordbreaker(context));
      assert.deepEqual(spans.map((s) => s.text), ['']);
    });

    it('properly breaks a context with text tokens and a context-final space', () => {
      const context = '  apple ';
      const spans = breaker(context);
      assert.deepEqual(spans.map((s) => new LazySpan(context, s.start, s.end)), defaultWordbreaker(context));
      assert.deepEqual(spans.map((s) => s.text), ['apple', '']);
    });

    it('properly handles tokens with non-BMP text', () => {
      const context = `  ${mixedPlaneApple} `;
      const spans = breaker(context);
      assert.deepEqual(spans.map((s) => new LazySpan(context, s.start, s.end)), defaultWordbreaker(context));
      assert.deepEqual(spans.map((s) => s.text), [mixedPlaneApple, '']);
    });
  });

  describe('ait.mnw.mon 1.1', () => {
    const breaker = sanitizeResults(simpleNaiveCustomBreaker);

    this.beforeAll(() => {
      assert.isOk(breaker);
    });

    it('properly breaks a context with two spaces', () => {
      const spans = breaker('  ');
      assert.deepEqual(spans.map((s) => s.text), ['']);
      assert.deepEqual(spans.map((s) => s.start), ['  '.length]);
    });

    it('properly breaks a context with text tokens and a context-final space', () => {
      const spans = breaker('  ကခဗံၚ် ');
      assert.deepEqual(spans.map((s) => s.text), ['ကခဗံၚ်', '']);
      assert.deepEqual(spans.map((s) => s.start), ['  '.length, '  ကခဗံၚ် '.length]);
    });

    it('properly breaks a context with a single-char final token', () => {
      const spans = breaker('ကၚ လိက်အုပ် အ');
      assert.deepEqual(spans.map((s) => s.text), ['ကၚ', 'လိက်အုပ်', 'အ']);
      assert.deepEqual(spans.map((s) => s.start), [''.length, 'ကၚ '.length, 'ကၚ လိက်အုပ် '.length]);
    });

    it('properly handles tokens with non-BMP text', () => {
      const spans = breaker(`  ${mixedPlaneApple} `);
      assert.deepEqual(spans.map((s) => s.text), [mixedPlaneApple, '']);
      assert.deepEqual(spans.map((s) => s.start), ['  '.length, `  ${mixedPlaneApple} `.length]);
    });
  });

  describe('sil.km.gcc 2.0', () => {
    const breaker = sanitizeResults(extendedCustomBreaker);

    this.beforeAll(() => {
      assert.isOk(breaker);
    });

    it('properly breaks a context with two spaces', () => {
      const spans = breaker('  ');
      assert.deepEqual(spans.map((s) => s.text), ['']);
      assert.deepEqual(spans.map((s) => s.start), ['  '.length]);

    });

    it('properly breaks a context with text tokens and a context-final space', () => {
      const spans = breaker('  ការ ');
      assert.deepEqual(spans.map((s) => s.text), ['ការ', '']);
      assert.deepEqual(spans.map((s) => s.start), ['  '.length, '  ការ '.length]);
    });

    it('properly handles tokens with non-BMP text', () => {
      const spans = breaker(`  ${mixedPlaneApple} `);
      assert.deepEqual(spans.map((s) => s.text), [mixedPlaneApple, '']);
      assert.deepEqual(spans.map((s) => s.start), ['  '.length, `  ${mixedPlaneApple} `.length]);
    });
  });
});
