/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Verify email address format, following WHATWG guidelines
 */

// There is no "good" definition of a valid email address. Email addresses are
// horrific. They can contain comments, whitespace, and all manner of ugly
// things. Because we use AJV to verify JSON files, we use their specification
// on what is a valid email address. Some useful references:
// * https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address
// * http://stackoverflow.com/questions/201323/using-a-regular-expression-to-validate-an-email-address#answer-8829363
// * https://github.com/ajv-validator/ajv-formats/blob/4ca86d21bd07571a30178cbb3714133db6eada9a/src/formats.ts#L122
// * https://github.com/ajv-validator/ajv-formats/blob/4ca86d21bd07571a30178cbb3714133db6eada9a/src/formats.ts#L65

export function isValidEmail(email: string) {
  return /^[a-z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\.[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?)*$/i.test(email);
}