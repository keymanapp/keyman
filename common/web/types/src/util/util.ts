/**
 * xml2js will not place single-entry objects into arrays. Easiest way to fix
 * this is to box them ourselves as needed. Ensures that o.x is an array.
 *
 * @param o Object with property to box
 * @param x Name of element to box
 */
export function boxXmlArray(o: any, x: string): void {
  if(typeof o == 'object' && !Array.isArray(o[x])) {
    if(o[x] === null || o[x] === undefined) {
      o[x] = [];
    }
    else {
      o[x] = [o[x]];
    }
  }
}

// TODO-LDML: #7569 the below regex works, but captures more than it should
// (it would include \u{fffffffffffffffff } which
// is overlong and has a space at the end.) The second regex does not work yet.
const MATCH_HEX_ESCAPE = /\\u{([0-9a-fA-F ]{1,})}/g;
// const MATCH_HEX_ESCAPE = /\\u{((?:(?:[0-9a-fA-F]{1,5})|(?:10[0-9a-fA-F]{4})(?: (?!}))?)+)}/g;

export class UnescapeError extends Error {
}

/**
 * Unescapes a string according to UTS#18§1.1, see <https://www.unicode.org/reports/tr18/#Hex_notation>
 * @param s escaped string
 * @returns
 */
export function unescapeString(s: string): string {
  if(!s) {
    return s;
  }
  try {
    /**
     * Unescape one codepoint
     * @param hex one codepoint in hex, such as '0127'
     * @returns the unescaped codepoint
     */
    function unescapeOne(hex: string) : string {
      const codepoint = Number.parseInt(hex, 16);
      return String.fromCodePoint(codepoint);
    }
    /**
     * process one regex match
     * @param str ignored
     * @param matched the entire match such as '0127' or '22 22'
     * @returns the unescaped match
     */
    function processMatch(str: string, matched: string) : string {
      const codepoints = matched.split(' ');
      const unescaped = codepoints.map(unescapeOne);
      return unescaped.join('');
    }
    s = s.replaceAll(MATCH_HEX_ESCAPE, processMatch);
  } catch(e) {
    if (e instanceof RangeError) {
      throw new UnescapeError(`Out of range while unescaping '${s}': ${e.message}`, { cause: e });
    } else {
      throw e;
    }
  }

  return s;
}

/**
 * This function operates similarly to Node's path.basename(); unlike
 * path.basename(), it currently requires the ext parameter.
 * @param name a pathname, which should end in ext, e.g. "/tmp/file.xml" or "C:\temp\file.xml"
 * @param ext  a file extension. including initial period, such as ".xml"
 * @returns the base name without path or extension
 */
export function basename(name: string, ext: string) {
  const basenameRegexp = new RegExp("([\\/\\\\]|^)([^\\/\\\\]+)\\"+ext+"$", "i");
  const m = name.match(basenameRegexp);
  if(!m) {
    return null;
  }
  return m[2];
}