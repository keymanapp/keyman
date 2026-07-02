/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';

// Also relies on string-extensions provided by the web-utils package.

/**
 * Determines whether a lexical model transform is empty (has no effect).
 *
 * @param transform - The lexical model transform to check.
 * @returns True if the transform is empty (null, or has no insertions or deletions),
 *          false otherwise.
 */
export function isEmptyTransform(transform: LexicalModelTypes.Transform) {
  if (!transform) {
    return true;
  }
  return transform.insert === '' && transform.deleteLeft === 0 && (transform.deleteRight ?? 0) === 0;
}
