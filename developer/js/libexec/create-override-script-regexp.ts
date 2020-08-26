import {readFileSync} from "fs";
import * as path from "path";

// Where to find UnicodeData.txt and Blocks.txt
const UCD_DIR = path.join("..", "..", "resources", "standards-data", "unicode-character-database");

const SPACELESS_SCRIPT_BLOCKS = new Set([
  "Myanmar",
  "Lao",
  "Thai",
  "Khmer",
  "Katakana",
  "Katakana Phonetic Extensions",
]);

let blockIter = blocks();
let block = nextBlock()

let elligibleCharacters = [];

// @ts-ignore
for (let {codePoint, generalCategory} of unicodeData()) {
  if (!block.contains(codePoint)) {
    block = nextBlock();
  }
  console.assert(block.contains(codePoint));

  if (!SPACELESS_SCRIPT_BLOCKS.has(block.name)) {
    continue;
  }
  if (!isLetterOrMark(generalCategory)) {
    continue;
  }

  elligibleCharacters.push(codePoint)
}

let ranges = [];
let previousCharacter = elligibleCharacters.shift()
let currentRange = [previousCharacter, previousCharacter];
for (let cp of elligibleCharacters) {
  if (cp === previousCharacter + 1) {
    currentRange[1] = cp;
  } else {
    ranges.push(currentRange);
    currentRange = [cp, cp];
  }

  previousCharacter = cp;
}

let characterClasses = ranges.map(([lower, upper]) => {
  if (lower === upper) {
    return uplus(lower);
  } else if (lower == upper - 1) {
    return uplus(lower) + uplus(upper);
  } else {
    return `${uplus(lower)}-${uplus(upper)}`;
  }
}).join("");

console.log(`/**
 * AUTOMATICALLY GENERATED FILE. DO NOT MODIFY
 * See: libexec/create-override-script-regexp.ts for details!
 */
export const SOUTHEAST_ASIAN_LETTER = /^[${characterClasses}]$/;
`);

////////////////////////////////// Helpers ///////////////////////////////////

function* unicodeData() {
  let unicodeDataFile = readFileSync(path.join(UCD_DIR, "UnicodeData.txt"), "UTF-8");
  for (let line of unicodeDataFile.split("\n")) {
    if (line.trim() == "") {
      continue;
    }

    let parts = line.split(";")
    let data = {
      codePoint: parseInt(parts[0], 16),
      generalCategory: parts[2],
    }

    yield data;
  }
}

function* blocks() {
  let blocksFile = readFileSync(path.join(UCD_DIR, "Blocks.txt"), "UTF-8");
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

function uplus(cp: number) {
  let hex = cp.toString(16).toUpperCase();
  let leadingZeros = "0".repeat(4 - hex.length);
  return `\\u${leadingZeros}${hex}`;
}
