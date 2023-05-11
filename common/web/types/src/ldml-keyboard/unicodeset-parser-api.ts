
/**
 * Abstraction for an interface capable of parsing a UnicodeSet
 */
export interface UnicodeSetParser {
  parseUnicodeSet(pattern: string, bufferSize: number) : UnicodeSet | null;
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
