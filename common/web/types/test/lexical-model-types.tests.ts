/**
 * This file "tests" the exports from the main module.
 *
 * Since the exports are all types, the "test" here is that the type can be
 * imported and compiled without any compiler errors.
 */

import { KMXPlus, ElementString, LexicalModelTypes } from "@keymanapp/common-types";

export let u: LexicalModelTypes.USVString;
export let l: LexicalModelTypes.Transform;
export let s: LexicalModelTypes.Suggestion;
export let st: LexicalModelTypes.SuggestionTag;
export let c: LexicalModelTypes.Context;
export let cap: LexicalModelTypes.Capabilities;
export let conf: LexicalModelTypes.Configuration;
export let d: LexicalModelTypes.Distribution<LexicalModelTypes.Suggestion>;
export let wbf: LexicalModelTypes.WordBreakingFunction;
export let sp: LexicalModelTypes.Span;
export let lmp: LexicalModelTypes.LexicalModelPunctuation;


// try some of the other types - that should still work
export let elemString: ElementString;
export let section: KMXPlus.Section;
