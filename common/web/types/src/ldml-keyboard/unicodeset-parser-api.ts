
/**
 * Abstraction for an interface capable of parsing a UnicodeSet
 */
export interface UnicodeSetParser {
  /**
   * Parse a UnicodeSet into ranges
   * @param pattern string to parse such as `[a-z]`
   * @param rangeCount number of ranges to allow for
   */
  parseUnicodeSet(pattern: string, rangeCount: number) : UnicodeSet | null;
  /**
   * Calculate the number of ranges in a UnicodeSet
   * @param pattern string to parse such as `[a-z]`
   * @returns number of ranges, or -1 (with callback-reported err) on err
   */
  sizeUnicodeSet(pattern: string) : number;
}

/**
 * Parsed UnicodeSet, return value of of parseUnicodeSet()
 */
export class UnicodeSet {
  /**
   * A UnicodeSet in range form.
   * For example, `[ħa-z]` will parse to
   * ranges = `[[0x41, 0x7A], [0x0127, 0x0127]]` meaning `[a,z], [ħ,ħ]`
   * @param pattern the pattern per UnicodeSet syntax such as `[a-z]`
   * @param ranges array of 2-element arrays, 'start' and 'end'.
   */
  constructor(public pattern: string, public ranges: number[][]) {
  }
  /**
   * Number of ranges
   */
  get length() : number {
    return this.ranges.length;
  }

  toString() : string {
    return this.pattern;
  }
}
