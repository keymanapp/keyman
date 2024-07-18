/**
 * Prints a JavaScript regular expression suitable for use in the
 * **overrideScriptDefaults** word breaker decorator.
 *
 * This regular expression matches letters or marks in scripts that
 * conventionally use spaces. This way, the word breaker can make sure to keep
 * contiguous spans of these characters together!
 *
 * If you need to add more Unicode blocks, customize SPACELESS_SCRIPT_BLOCKS.
 */

import {readFileSync} from "fs";
import * as path from "path";

// Where to find UnicodeData.txt and Blocks.txt
const UCD_DIR = path.join("..", "..", "..", "..", "resources", "standards-data", "unicode-character-database");

const SPACELESS_SCRIPT_BLOCKS = new Set([
  "Myanmar",  // a.k.a., Burmese
  "Lao",
  "Thai",
  "Khmer",
  "Katakana",
  "Katakana Phonetic Extensions",
  // Add more scripts here, as necessary!
]);

let blockIter = blocks();
let block = nextBlock();

let eligibleCharacters: number[] = [];

// @ts-ignore: TypeScript complains that it can't compile for..of over a
// generator without --downlevelIteration, but it works anyway?
for (let {codePoint, generalCategory} of unicodeData()) {
  if (!block.contains(codePoint)) {
    block = nextBlock();
  }
  console.assert(block.contains(codePoint));

  if (SPACELESS_SCRIPT_BLOCKS.has(block.name) && isLetterOrMark(generalCategory)) {
    eligibleCharacters.push(codePoint);
  }
}

const ranges = groupCodePointsIntoRanges(eligibleCharacters);

const characterClasses = ranges.map(([lower, upper]) => {
  if (lower === upper) {
    return unicodeEscape(lower);
  } else if (lower === upper - 1) {
    return unicodeEscape(lower) + unicodeEscape(upper);
  } else {
    return `${unicodeEscape(lower)}-${unicodeEscape(upper)}`;
  }
}).join("");

console.log(`/**
 * AUTOMATICALLY GENERATED FILE. DO NOT MODIFY
 * See: libexec/create-override-script-regexp.ts for details!
 */
export const HAS_SOUTHEAST_ASIAN_LETTER = /[${characterClasses}]/;`);


////////////////////////////////// Helpers ///////////////////////////////////

function* unicodeData() {
  let unicodeDataFile = readFileSync(path.join(UCD_DIR, "UnicodeData.txt"), "utf8");
  for (let line of unicodeDataFile.split("\n")) {
    if (line.trim() == "") {
      continue;
    }

    let parts = line.split(";")
    yield {
      codePoint: parseInt(parts[0], 16),
      generalCategory: parts[2],
    }
  }
}

function* blocks() {
  let blocksFile = readFileSync(path.join(UCD_DIR, "Blocks.txt"), "utf8");
  for (let line of blocksFile.split("\n")) {
    if (line.trim() === "") {
      continue;
    }
    if (line.startsWith("#")) {
      continue;
    }

    let [range, name] = line.split("; ");
    let [lower, upper] = range.split("..").map(s => parseInt(s, 16));

    yield {
      name, lower, upper,
      contains(codePoint: number) {
        return lower <= codePoint && codePoint <= upper;
      }
    }
  }
}

function nextBlock() {
  let {value} = blockIter.next();
  if (!value) {
    throw new Error("ran out of blocks");
  }
  return value;
}

function isLetterOrMark(category: string): boolean {
  return category.startsWith("L") || category.startsWith("M");
}

function groupCodePointsIntoRanges(characters: number[]): [number, number][] {
  let ranges: [number,number][] =  [];

  let previousCharacter = characters[0];
  const candidates = characters.slice(1);
  let currentRange: [number, number] = [previousCharacter, previousCharacter];
  for (let codePoint of candidates) {
    if (codePoint === previousCharacter + 1) {
      currentRange[1] = codePoint;
    } else {
      ranges.push(currentRange);
      currentRange = [codePoint, codePoint];
    }

    previousCharacter = codePoint;
  }

  return ranges;
}

function unicodeEscape(codePoint: number) {
  if (codePoint > 0xFFFF) {
    throw new Error("non-BMP code points not supported");
  }

  let hex = codePoint.toString(16).toUpperCase().padStart(4,'0');
  return `\\u${hex}`;
}
