/**
 * This file "tests" the exports from the main module.
 *
 * Since the exports are all types, the "test" here is that the type can be
 * imported and compiled without any compiler errors.
 */

import { USVString, Transform, Suggestion, SuggestionTag, Context, Capabilities, Configuration, Distribution, WordBreakingFunction, Span, LexicalModelPunctuation, ElementString, KMXPlus } from '@keymanapp/common-types';

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


// try some of the other types - that should still work
export let elemString: ElementString;
export let section: KMXPlus.Section;
