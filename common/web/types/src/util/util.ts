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

const MATCH_HEX_ESCAPE = /\\u{([0-9a-fA-F]{1,5})}/g;

export function unescapeString(s: string): string {
  if(!s) return s;

  s = s.replaceAll(MATCH_HEX_ESCAPE, (str,hex) => String.fromCodePoint(Number.parseInt(hex, 16)));

  return s; // null imp
}
