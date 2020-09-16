/// <reference path="./index.d.ts" />

/**
 * This file "tests" the exports from the main module.
 * 
 * Since the exports are all types, the "test" here is that the type can be
 * imported and compiled without any compiler errors.
 */

export let u: USVString;
export let l: Transform
export let s: Suggestion;
export let st: SuggestionTag;
export let c: Context;
export let cap: Capabilities;
export let conf: Configuration;
export let d: Distribution<Suggestion>;
export let wbf: WordBreakingFunction;
export let sp: Span;
export let lmp: LexicalModelPunctuation;