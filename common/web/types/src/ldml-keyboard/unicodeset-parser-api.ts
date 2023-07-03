
/**
 * Abstraction for an interface capable of parsing a UnicodeSet
 */
export interface UnicodeSetParser {
  /**
   * Parse a UnicodeSet into ranges
   * @param pattern string to parse such as `[a-z]`
   * @param bufferSize number of ranges to allow for
   */
  parseUnicodeSet(pattern: string, bufferSize: number) : UnicodeSet | null;
  /**
   * Calculate the number of ranges in a UnicodeSet
   * @param pattern string to parse such as `[a-z]`
   * @returns number of ranges, or -1 (with callback-reported err) on err
   */
  sizeUnicodeSet(pattern: string) : number;
}

/**
 * Represents a parsed UnicodeSet
 */
export class UnicodeSet {
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
