#!/usr/bin/env node

// Original version found at: https://github.com/eddieantonio/unicode-default-word-boundary/blob/master/libexec/compile-word-break.js

import fs from 'fs';
import path from 'path';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

/*
 * Generates the TypeScript file for data required for the word boundary
 * function:
 *
 *  - a sorted array to facilitate binary search of the Word_Break property.
 *  - a regular expression that matches characters that have Extended_Pictographic=Yes.
 *
 * For internal use only. Please keep away from children.
 *
 * The generated file is saved to ../src/gen/WordBreakProperty.ts
 */

const MAX_CODE_POINT = 0x10FFFF;

// Where to get the data:
//  - https://www.unicode.org/reports/tr51/#emoji_data
//  - https://www.unicode.org/reports/tr41/#Props0

//////////////////////////////////// Main ////////////////////////////////////

const projectDir = path.dirname(require.resolve("@keymanapp/models-wordbreakers/README.md"));
const generatedFilename = path.join(projectDir, 'src', 'main', 'default', 'data.ts');

// The data files should be in this repository, with names matching the
// Unicode version.
const wordBoundaryFilename = path.join(projectDir, `../../../resources/standards-data/unicode-character-database/WordBreakProperty.txt`);
const emojiDataFilename = path.join(projectDir, `../../../resources/standards-data/unicode-character-database/emoji-data.txt`);

///////////////////////////// Word_Boundary file /////////////////////////////

interface DataRange {
  start: number;
  end: number;
  property: string;
}

// Extract the ranges IN ASCENDING ORDER from the file.
// This will be the big binary search table.
let ranges = readCharacterPropertyFile(wordBoundaryFilename)
  .sort((a, b) => {
    return a.start - b.start;
  });

// The list of ranges are initially sparse — having gaps between assigned
// ranges. Fill in those gaps:
ranges = makeDense(ranges);
ensureDense(ranges);

// The possible Word_Break property assignments.
let categories = new Set<string>();
for (let {property} of ranges) {
  categories.add(property);
}
// Also add pseudo-categories of start-of-text and end-of-text:
categories.add('sot');
categories.add('eot');

///////////////////////// Extended_Pictographic=Yes //////////////////////////

let extendedPictographicCodePoints = readCharacterPropertyFile(emojiDataFilename)
  .filter(({property}) => property === 'Extended_Pictographic');

// Try generating the regular expression both in a way that is
// backwards-compatbile and one that only works in ES6+.
// let extendedPictographicRegExp;
let compatibleRegexp = utf16AlternativesStrategy();
let es6Regexp = unicodeRangeStrategy();

// Choose the shortest regular expression.
// In my experience, the ES6 regexp is an order of magnitude smaller!
if (es6Regexp.length < compatibleRegexp.length) {
  // extendedPictographicRegExp = es6Regexp;
  console.warn(`Using ES6 regexp [${es6Regexp.length} chars]`);
} else {
  // extendedPictographicRegExp = compatibleRegexp;
  console.warn(`Using compatibility regexp [${compatibleRegexp.length} chars]`);
}

let catIndexSeed = 0;
const categoryMap = new Map<string, number>();

for(let cat of categories) {
  categoryMap.set(cat, catIndexSeed++);
  if(catIndexSeed == '`'.charCodeAt(0)) {
    catIndexSeed++; // Skip the back-tick as an encoding symbol.
                    // Reduces complications, as it's the encoding string start/end char.
  }
}

const bmpRanges: typeof ranges = [];
const nonBmpRanges: typeof ranges = [];

// { start: number, property: number}[]
for(let range of ranges) { // already sorted
  if(range.start <= 0xFFFF) {
    bmpRanges.push(range);
  } else {
    if(nonBmpRanges.length == 0) {
      const finalBmpRange = bmpRanges[bmpRanges.length - 1];
      bmpRanges.push({
        start: 0xFFFF,
        property: range.property,
        end: undefined
      });

      if(range.start != 0x10000) {
        nonBmpRanges.push({
          start: 0x10000,
          property: finalBmpRange.property,
          end: undefined
        });
      }
    }

    nonBmpRanges.push(range);
  }
}

//////////////////////// Creating the generated file /////////////////////////

// Save the output in the gen/ directory.
let stream = fs.createWriteStream(generatedFilename);

function escape(codedChar: string) {
  if(codedChar == '`' || codedChar == '\\') {
    return '\\' + codedChar;
  } else {
    return codedChar;
  }
}

// // Former entry in the original version by Eddie that was never included in our repo:
// export const extendedPictographic = ${extendedPictographicRegExp};

// Generate the file!
stream.write(`// Automatically generated file. DO NOT MODIFY.
// The generator script is defined at /common/models/wordbreakers/src/data-compiler/index.ts.

/**
 * Valid values for a word break property.
 *
 * Is optimized away at compile-time; use \`propertyMap\` to find the mapped
 * value at runtime for a property name if needed.
 */
export const enum WordBreakProperty {
${ /* Create enum values for each word break property */
  Array.from(categories)
    .map(x => `  ${x} = ${categoryMap.get(x)}`)
    .join(',\n')
}
};

/**
 * Contains property names per associated index, as this is compiled away
 * by TypeScript for \`const enum\` cases like \`WordBreakProperty\`.
 */
export const propertyMap = [
${ /* Enumerate the plain-text names for ease of lookup at runtime */
  Array.from(categories)
  .map(x => `  "${x}"`)
  .join(',\n')
}
];

export const WORD_BREAK_PROPERTY_BMP: string = \`${
  // To consider:  emit `\uxxxx` codes instead of the raw char?
  bmpRanges.map(({start, property}) => {
    let codedStart = escape(String.fromCodePoint(start));

    // Offset the encoded property value to lie within a friendlier range,
    // with characters that render naturally within code editors.
    const codedProp = escape(String.fromCharCode(categoryMap.get(property) + 0x20));
    return `${codedStart}${codedProp}`;
  }).join('')
}\`;

export const WORD_BREAK_PROPERTY_NON_BMP: string = \`${
  // To consider:  emit `\uxxxx` codes instead of the raw char?
  nonBmpRanges.map(({start, property}) => {
    const codedStart = escape(String.fromCodePoint(start));

    // Offset the encoded property value to lie within a friendlier range,
    // with characters that render naturally within code editors.
    const codedProp = escape(String.fromCharCode(categoryMap.get(property) + 0x20));
    return `${codedStart}${codedProp}`;
  }).join('')
}\`;
`);

/**
 * Reads a Unicode character property file.
 *
 * Character property files are composed of comment lines, empty lines, and
 * property lines.  Comments lines begin with '#' and should be ignored, as
 * well as empty lines.
 *
 * Property lines have a code point or a code point range, followed by a
 * semi-colon, followed by the property text. e.g.,
 *
 *    1F600         ; Emoji                #  6.1  [1] (😀)        grinning face
 *    26C4..26C5    ; Emoji_Presentation   #  5.2  [2] (⛄..⛅)    snowman without snow..sun behind butt
 *
 * This will read the file at the given filename, and return an ordered array
 * or property lines, with attributes:
 *
 *  {start: number, end: number, property: string}
 *
 * If the property specifies a single code point (i.e., not a range of code
 * points), then end === start.
 */
function readCharacterPropertyFile(filename: string) {
  let textContents = fs.readFileSync(filename, { encoding: 'utf8'});

  return textContents.split('\n')
    .filter(line => !line.startsWith('#') && line.trim())
    .map(line => {
      let [_, startText, endText, property] = line.match(
        // Parse lines that look like this:
        // 0000             .. 0000               ;   CategoryName
        /^([0-9A-F]{4,6})(?:..([0-9A-F]{4,6}))?\s+;\s+([A-Za-z_]+)/
      );

      let start = parseCodepoint(startText);
      let end = endText !== undefined ? parseCodepoint(endText) : start;

      return { start, end, property };
    });
}

/**
 * Parses a code point, expressed as a 4 or 6 digit hexadecimal string.
 * Does some bounds checking in order to determine if the string is in fact a
 * valid code point.
 */
function parseCodepoint(hexString: string) {
  let number = parseInt(hexString, 16);
  if (Number.isNaN(number)) {
    throw new SyntaxError(`Cannot parse codepoint: ${hexString}`);
  }

  if (number < 0 || number > MAX_CODE_POINT) {
    throw new RangeError(`Codepoint out of range: ${number}`);
  }

  return number;
}

function toUnicodeEscape(codePoint: number) {
  let isBMP = codePoint <= 0xFFFF;
  let simpleConversion = codePoint.toString(16).toUpperCase();

  let padding = (isBMP ? 4 : 6) - simpleConversion.length;
  let digits = '0'.repeat(padding) + simpleConversion;

  if (isBMP) {
    return '\\u' + digits;
  } else {
    return `\\u{${digits}}`;
  }
}

function utf16AlternativesStrategy() {
  let codePoints = [];
  for (let {start, end} of extendedPictographicCodePoints) {
    for (let current = start; current <= end; current ++) {
      codePoints.push(current);
    }
  }

  let alternatives = codePoints.map(codePointToUTF16Escape);
  return `/^(?:${alternatives.join('|')})/`;
}

function codePointToUTF16Escape(codePoint: number): string {
  // Scalar values remain the same
  if (codePoint <= 0xFFFF) {
    return toUnicodeEscape(codePoint);
  }

  const LOWEST_TEN_BITS_MASK = 0x03FF;
  let astralBits = codePoint - 0x10000;

  let highSurrogate = 0xD800 + (astralBits >>> 10);
  let lowSurrogate = 0xDC00 + (astralBits & LOWEST_TEN_BITS_MASK);

  console.assert(highSurrogate <= 0xDBFF);
  console.assert(lowSurrogate <= 0xDFFF);
  console.assert(String.fromCharCode(highSurrogate) + String.fromCharCode(lowSurrogate) ===
                 String.fromCodePoint(codePoint));
  return codePointToUTF16Escape(highSurrogate) + codePointToUTF16Escape(lowSurrogate);
}

function unicodeRangeStrategy() {
  let regexp = '';
  for (let {start, end} of extendedPictographicCodePoints) {
    if (start === end) {
      regexp += toUnicodeEscape(start);
    } else {
      regexp += toUnicodeEscape(start) + '-' + toUnicodeEscape(end);
    }
  }
  return `/^[${regexp}]/u`;
}

function makeDense(ranges: DataRange[]) {
  return joinSameAdjacentProperties(fillInGaps(ranges));
}

function ensureDense(ranges: DataRange[]) {
  let lastEnd = -1;
  let lastProperty = 'sot';
  for (let range of ranges) {
    let {start, end, property} = range

    if (lastEnd + 1 !== start) {
      throw new Error(`Non-adjacent range: ${JSON.stringify(range)}`);
    }

    if (lastProperty === property) {
      throw new Error(`adjacent ranges have same property: ${JSON.stringify(range)}`);
    }

    lastEnd = end;
    lastProperty = property;
  }
}


function joinSameAdjacentProperties(ranges: DataRange[]) {
  console.assert(ranges.length > 1);

  let conjoinedRanges = [];
  conjoinedRanges.push(ranges.shift());

  for (let range of ranges) {
    let lastRange = conjoinedRanges[conjoinedRanges.length - 1];
    if (range.property === lastRange.property) {
      lastRange.end = range.end;
    } else {
      conjoinedRanges.push(range);
    }
  }

  return conjoinedRanges;
}

function fillInGaps(ranges: DataRange[]) {
  console.assert(ranges.length > 1);

  let denseRanges = [];
  let nextUnaccountedCodepoint = 0x0000;

  for (let range of ranges) {
    if (range.start > nextUnaccountedCodepoint) {
      // Need to create a range BEFORE the next start of ranges
      denseRanges.push({
        start: nextUnaccountedCodepoint,
        end: range.start - 1,
        // If it's unassigned in the file, it should be 'Other'.
        property: 'Other',
      });
    }

    denseRanges.push(range);
    nextUnaccountedCodepoint = range.end + 1;
  }

  // Create the last range (till the end)
  if (nextUnaccountedCodepoint < MAX_CODE_POINT) {
    denseRanges.push({
      start: nextUnaccountedCodepoint,
      end: MAX_CODE_POINT,
      property: 'Other',
    })
  }

  return denseRanges;
}