import {
  SENTINEL_CODE_UNIT, applyTransform, buildMergedTransform, isHighSurrogate, isLowSurrogate, isSentinel,
  transformToSuggestion, defaultApplyCasing
} from "./common.js";
import PriorityQueue, { Comparator } from "./priority-queue.js";
import QuoteBehavior from "./quote-behavior.js";
import { Tokenization, tokenize, getLastPreCaretToken, wordbreak } from "./tokenization.js";
import TrieModel, { TrieModelOptions } from "./trie-model.js";

import { extendString } from "@keymanapp/web-utils";

// This package requires our string-extension functions.
extendString();

export {
  SENTINEL_CODE_UNIT, applyTransform, buildMergedTransform, isHighSurrogate, isLowSurrogate, isSentinel,
  transformToSuggestion, defaultApplyCasing, // "common.ts"
  PriorityQueue, Comparator, // "priority-queue.ts"
  QuoteBehavior, // "quote-behavior.ts",
  Tokenization, tokenize, getLastPreCaretToken, wordbreak, // "tokenization.ts"
  TrieModel, TrieModelOptions // "trie-model.ts"
};
