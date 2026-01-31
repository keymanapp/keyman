/**
 * Quite useful for for facilitating pre-processing of eventually-readonly
 * configurations during class initializations.
 */
export type Mutable<Type> = {
  -readonly [Property in keyof Type]: Type[Property];
};