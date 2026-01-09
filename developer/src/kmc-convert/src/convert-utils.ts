
/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Created by S. Schmitt on 2025-08-24
 *
 * functions for kmc-convert utils
 *
 */

/**
 * @brief  function to convert a numeric character reference or a unicode value to a unicode character e.g. &#x63 -> c;  U+1F60E -> 😎
 * @param  inputString the value that will converted
 * @return a unicode character like 'c', 'ሴ', '😎' or undefined if inputString is not recognized
 */
export function convertToUnicodeCharacter(inputString: string): string | undefined {

  let m: RegExpMatchArray | null;

  if ((inputString === null) || (inputString === undefined)) {
    return undefined;
  }

  // e.g. U+0061 U+1234 U+1F60E
  m = inputString.match(/^U\+([0-9a-f]{2,6})$/i);
  if (m) return String.fromCodePoint(parseInt(m[1], 16));

  // e.g. &#x61;  &#x1234; &#x1F60E;
  m = inputString.match(/^&#x([0-9a-f]{2,6});$/i);
  if (m) return String.fromCodePoint(parseInt(m[1], 16));

  // e.g. &#97; &#4660; &#128518;
  m = inputString.match(/^&#([0-9]{2,6});$/);
  if (m) return String.fromCodePoint(parseInt(m[1], 10));

  // e.g. &gt; &quot;
  m = inputString.match(/^&([a-z]{2,4});$/i);
  if (m) {
    switch (inputString) {
      case '&gt;': return '>';
      case '&lt;': return '<';
      case '&amp;': return '&';
      case '&apos;': return "'";
      case '&quot;': return '"';
      default: return undefined;
    }
  }

  // 'A'  or  "B" have length=1 and segment-length=1 and will be used.
  // "ẘ"  or  "😎" have length=2 but segment-length=1 and will be used.
  // "ab" has length=2 and segment-length=2 and will not be used.
  else if ([...new Intl.Segmenter().segment(inputString)].length <= 1) {
    return inputString;
  }
  else {
    return undefined;
  }
}

/**
 * @brief  function to convert a numeric character reference to a unicode Code Point e.g. &#4660 -> U+1234;  &#x10F601 -> U+1F60E
 * @param  instr the value that will converted
 * @return returns a unicode Code Point like U+0063, U+1234, U+1F60E; returns the input character if a non-numeric reference is used or returns 'undefined' if instr is not recognized
 */
export function convertToUnicodeCodePoint(instr: string): string {

  if ((instr === null) || (instr === undefined)) {
    return undefined;
  }
  if (instr.startsWith("&#x")) {
    const numLength = instr.length - 3;
    const numStr = instr.substring(3, instr.length - 1);
    return ("U+" + numStr.slice(-numLength).padStart(4, "0"));
  }

  // if not hex: convert to hex
  if (instr.startsWith("&#")) {
    const numLength = instr.length - 2;
    const numStr = instr.substring(2, instr.length - 1);
    return "U+" + Number(numStr.slice(-numLength)).toString(16).slice(-6).toUpperCase().padStart(4, "0");
  }
  else
    return instr;
}

