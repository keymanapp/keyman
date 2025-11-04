import { LexicalModelTypes } from '@keymanapp/common-types';

// Also relies on string-extensions provided by the web-utils package.

export function isEmptyTransform(transform: LexicalModelTypes.Transform) {
  if (!transform) {
    return true;
  }
  return transform.insert === '' && transform.deleteLeft === 0 && (transform.deleteRight ?? 0) === 0;
}
