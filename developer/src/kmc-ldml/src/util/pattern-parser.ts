/**
 * Utilities for transform and marker processing
 */


/**
 * Helper function for extracting matched items
 * @param str input string
 * @param match global RegEx to use
 * @returns array of matched values
 */
function matchArray(str: string, match: RegExp) : string[] {
  const refs = (str || '').matchAll(match);
  return Array.from(refs).map(r => r[1]);
}

/**
 * Class for helping with markers
 */
export class MarkerParser {
  /**
   * A marker id has the same constraint as a key id. TODO-LDML: Needs to be reflected in the spec
   */
  public static readonly ID = /^[0-9A-Za-z_]{1,32}$/;

  /**
   * Special marker reference referring to any marker
   */
  public static readonly ANY_MARKER = '\\m{.}';

  /**
   * id of the 'any' marker
   */
  public static readonly ANY_MARKER_ID = '.';

  /**
   * Pattern for matching a marker reference, OR the special marker \m{.}
   */
  public static readonly REFERENCE = /\\m{([0-9A-Za-z_]{1,32}|\.)}/g;

  /**
   * parse a string into marker references
   * @param str input string such as "\m{a} … \m{.}"
   * @returns `[]` or an array of all markers referenced
   */
  public static allReferences(str: string): string[] {
    return matchArray(str, this.REFERENCE);
  }
}

/**
 * Class for helping with markers
 */
export class VariableParser {
  /**
   * A marker id has the same constraint as a key id. TODO-LDML: Needs to be reflected in the spec
   */
  public static readonly ID = /^[0-9A-Za-z_]{1,32}$/;

  /**
   * Pattern for matching a string reference `$(str)`
   */
  public static readonly STRING_REFERENCE = /\${([0-9A-Za-z_]{1,32})}/g;

  /**
   * Pattern for matching a set reference `$[set]`
   */
  public static readonly SET_REFERENCE = /\$\[([0-9A-Za-z_]{1,32})\]/g;

  /**
   * Pattern for matching a capture set reference `($[set])`
   */
  public static readonly CAPTURE_SET_REFERENCE = /\(\$\[([0-9A-Za-z_]{1,32})\]\)/g;

  /**
   * `$[1:variable]`
   * This regex matches the whole string.
   */
  public static readonly MAPPED_SET_REFERENCE = /^\$\[1:([0-9A-Za-z_]{1,32})\]$/;

  /**
   * parse a string into references
   * @param str input string
   * @returns `[]` or an array of all string references referenced
   */
  public static allStringReferences(str: string): string[] {
    return matchArray(str, this.STRING_REFERENCE);
  }

  /**
   * parse a string into references
   * @param str input string
   * @returns `[]` or an array of all string references referenced
   */
  public static allSetReferences(str: string): string[] {
    return matchArray(str, this.SET_REFERENCE);
  }

  /**
   * parse a string into references
   * @param str input string
   * @returns `[]` or an array of all string references referenced
   */
  public static allCaptureSetReferences(str: string): string[] {
    return matchArray(str, this.CAPTURE_SET_REFERENCE);
  }
}
