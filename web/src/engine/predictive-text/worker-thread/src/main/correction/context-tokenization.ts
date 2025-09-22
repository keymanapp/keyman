/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about one potential tokenization of contents of
 * the sliding context window for one specific instance of context state.
 */

import { Token } from '@keymanapp/models-templates';
import { LexicalModelTypes } from '@keymanapp/common-types';
import { KMWString } from '@keymanapp/web-utils';

import { ContextToken } from './context-token.js';
import TransformUtils from '../transformUtils.js';
import { computeAlignment, ContextStateAlignment } from './alignment-helpers.js';

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

// May be able to "get away" with 2 & 5 or so, but having extra will likely help
// with edit path stability.
const MIN_TOKENS_TO_RECONSIDER_FOR_TOKENIZATION = 3;
const MIN_CHARS_TO_RECONSIDER_FOR_TOKENIZATION = 8;

/**
 * This class represents the sequence of tokens (words and whitespace blocks)
 * held within the active sliding context-window at a single point in time.
 */
export class ContextTokenization {
  readonly tokens: ContextToken[];
  readonly alignment?: ContextStateAlignment;

  constructor(priorToClone: ContextTokenization);
  constructor(tokens: ContextToken[], alignment?: ContextStateAlignment);
  constructor(param1: ContextToken[] | ContextTokenization, alignment?: ContextStateAlignment) {
    if(!(param1 instanceof ContextTokenization)) {
      const tokens = param1;
      this.tokens = [].concat(tokens);
      this.alignment = alignment;
    } else {
      const priorToClone = param1;
      this.tokens = priorToClone.tokens.map((entry) => new ContextToken(entry));
      this.alignment = {...priorToClone.alignment};
    }
  }

  /**
   * Returns the token adjacent to the text insertion point.
   */
  get tail(): ContextToken {
    return this.tokens[this.tokens.length - 1];
  }

  /**
   * Returns plain-text strings representing the most probable representation for all
   * tokens represented by this tokenization instance.
   */
  get sourceText() {
    return this.tokens
      .filter(token => token.sourceText !== null)
      .map(token => token.sourceText);
  }

  /**
   * Returns a plain-text string representing the most probable representation for all
   * tokens represented by this tokenization instance.
   */
  get exampleInput(): string[] {
    return this.tokens
      // Hide any tokens representing invisible wordbreaks.  (Thinking ahead to phrase-level possibilities)
      .filter(token => token.exampleInput !== null)
      .map(token => token.exampleInput);
  }

  /**
   * Determines the alignment between a new, incoming tokenization source and the
   * tokenization modeled by the current instance.
   * @param incomingTokenization Raw strings corresponding to the tokenization of the incoming context
   * @param isSliding Notes if the context window is full (and sliding-alignment is particularly needed)
   * @param noSubVerify When true, this disables inspection of 'substitute' transitions that avoids
   * wholesale replacement of the original token.
   * @returns Alignment data that details if and how the incoming tokenization aligns with
   * the tokenization modeled by this instance.
   */
  computeAlignment(incomingTokenization: string[], isSliding: boolean, noSubVerify?: boolean): ContextStateAlignment {
    return computeAlignment(this.sourceText, incomingTokenization, isSliding, noSubVerify);
  }

  /**
   * Given an alignment between an incoming tokenization context and the current tokenization
   * instance, this method will produce a new ContextTokenization instance for the incoming context
   * that reuses as many correction-search intermediate results as possible.
   * @param tokenizedContext  A single tokenization for the incoming context that aligns well
   * with the tokenization represented by the current instance.
   * @param alignment The alignment, as determined by a prior call to `computeAlignment`.
   * @param lexicalModel The active lexical model
   * @param alignedTransformDistribution The tokenized version of the input distribution accounting
   * for the difference between the context represented by this instance and that of `tokenizedContext`.
   * @returns
   */
  transitionTo(
    tokenizedContext: Token[],
    alignment: ContextStateAlignment,
    lexicalModel: LexicalModel,
    // FUTURE NOTE:  especially for epic-dict-breaker, we'll want an array of these - to align across multiple transitions
    // in case word boundaries shift back and forth.
    alignedTransformDistribution: Distribution<Map<number, Transform>>
  ): ContextTokenization {
    if(!alignment.canAlign) {
      return null;
    }

    const {
      leadTokenShift,
      leadEditLength,
      matchLength,
      tailEditLength,
      tailTokenShift
    } = alignment;
    const hasDistribution = alignedTransformDistribution?.length > 0;

    // If we have a perfect match with a pre-existing tokenization, no mutations
    // have happened; just re-use the old context tokenization.
    if(leadEditLength == 0 && leadTokenShift == 0 && tailTokenShift == 0 && tailEditLength == 0) {
      // We must build a new instance in case the original did not have
      // alignment data (like when it's the initial context!)
      return new ContextTokenization(this.tokens, alignment);
    } else {
      // If we didn't get any input, we really should perfectly match
      // a previous context state.  If such a state is out of our cache,
      // it should simply be rebuilt.
      if(!hasDistribution) {
        return null;
      }
    }

    // If mutations HAVE happened, we have work to do.
    const tokenization = this.tokens.map((token) => new ContextToken(token));

    if(leadTokenShift < 0) {
      tokenization.splice(0, -leadTokenShift);
    } else if(leadTokenShift > 0) {
      // insert token(s) at the start to match the text that's back within the
      // sliding context window.

      const reinsertedTokens = tokenizedContext.slice(0, leadTokenShift);
      while(reinsertedTokens.length > 0) {
        const reinserted = reinsertedTokens.pop();
        const token = new ContextToken(lexicalModel, reinserted.text);
        tokenization.unshift(token);
      }
    }

    const incomingOffset = (leadTokenShift > 0 ? leadTokenShift : 0);
    const matchingOffset = (leadTokenShift < 0 ? -leadTokenShift : 0);

    // If a word is being slid out of context-window range, start trimming it - we should
    // no longer need to worry about reusing its original correction-search results.
    for(let i = 0; i < leadEditLength; i++) {
      if(this.tokens[matchingOffset+i].sourceText != tokenizedContext[incomingOffset+i].text) {
        //this.tokens[matchingOffset]'s clone is at tokenization[incomingOffset]
        //after the splice call in a previous block.
        tokenization[incomingOffset+i] = new ContextToken(lexicalModel, tokenizedContext[incomingOffset+i].text);
      }
    }

    // If no TAIL mutations have happened, we're safe to return now.
    if(tailEditLength == 0 && tailTokenShift == 0) {
      return new ContextTokenization(tokenization, alignment);
    }

    // first non-matched tail index within the incoming context
    const incomingTailUpdateIndex = matchLength + leadEditLength + incomingOffset;
    // first non-matched tail index in `matchState`, the base context state.
    const matchingTailUpdateIndex = matchLength + leadEditLength + matchingOffset;

    // The assumed input from the input distribution is always at index 0.
    const tokenizedPrimaryInput = hasDistribution ? alignedTransformDistribution[0].sample : null;

    // now that we've identified the 'primary input', sort the distributions.
    alignedTransformDistribution.sort((a, b) => b.p - a.p);

    // first index:  original sample's tokenization
    // second index:  token index within original sample
    const tokenDistribution = alignedTransformDistribution.map((entry) => {
      const remap: Map<number, ProbabilityMass<Transform>> = new Map();

      for(const pair of entry.sample.entries()) {
        remap.set(pair[0], {
          sample: pair[1],
          p: entry.p
        });
      }

      return remap;
    });

    // The original tail token should match index 0. If tokens have been deleted, that
    // left-shifts our base indices; we start left of 0. If more than one token was
    // edited, those edits occur to the left as well - and further left of whatever
    // the new tail token is *if* tokens were removed.
    const firstTailEditIndex = Math.min((1 - tailEditLength), 0) + Math.min(tailTokenShift, 0);
    for(let i = 0; i < tailEditLength; i++) {
      const tailIndex = firstTailEditIndex + i;

      // do tail edits
      const incomingIndex = i + incomingTailUpdateIndex;
      const matchingIndex = i + matchingTailUpdateIndex;

      const incomingToken = tokenizedContext[incomingIndex];
      const matchedToken = this.tokens[matchingIndex];

      let primaryInput = hasDistribution ? tokenizedPrimaryInput.get(tailIndex) : null;
      const isBackspace = primaryInput && TransformUtils.isBackspace(primaryInput);
      let token: ContextToken;

      if(isBackspace) {
        token = new ContextToken(lexicalModel, incomingToken.text);
        token.searchSpace.inputSequence.forEach((entry) => entry[0].sample.id = primaryInput.id);
      } else {
        // Assumption:  there have been no intervening keystrokes since the last well-aligned context.
        // (May not be valid with epic/dict-breaker or with complex, word-boundary crossing transforms)
        token = new ContextToken(matchedToken);

        // Erase any applied-suggestion transition ID; it is no longer valid.
        token.appliedTransitionId = undefined;
        const emptySample: ProbabilityMass<Transform> = { sample: { insert: '', deleteLeft: 0 }, p: 1 };
        token.addSourceInput(primaryInput ?? emptySample.sample);
        token.searchSpace.addInput(tokenDistribution.map((seq) => seq.get(tailIndex) ?? emptySample));
      }

      tokenization[incomingIndex] = token;
    }

    if(tailTokenShift < 0) {
      // delete tail tokens
      for(let i = 0; i > tailTokenShift; i--) {
        // If ALL that remains are deletes, we're good to go.
        //
        // This may not be the token at the index, but since all that remains are deletes,
        // we'll have deleted the correct total number from the end once all iterations
        // are done.
        tokenization.pop();
      }
    } else {
      // First tail insertion index within the tokenized-transform map is always 1.
      for(let i = 1; i <= tailTokenShift; i++) {
        // create tail tokens
        //                      same original base     after all edited
        const incomingIndex = incomingTailUpdateIndex + tailEditLength + (i - 1);
        const incomingToken = tokenizedContext[incomingIndex];
        // // Assertion:  there should be no matching token; this should be a newly-appended token.
        // const matchingIndex = i + tailEditLength + matchingTailUpdateIndex;

        const primaryInput = hasDistribution ? tokenizedPrimaryInput.get(i) : null;
        let pushedToken = new ContextToken(lexicalModel);

        // TODO:  assumes that there was no shift in wordbreaking for the actual
        // context when transitioning from the prior context to the current one.
        // This may actually be a major issue for dictionary-based wordbreaking!
        //
        // If there was such a shift, then we may have extra transforms
        // originally on a 'previous' token that got moved into this one!
        //
        // Suppose we're using a dictionary-based wordbreaker and have
        // `butterfl` for our context, which could become butterfly.  If the
        // next keystroke results in `butterfli`, this would likely be
        // tokenized `butter` `fli`.  (e.g: `fli` leads to `flight`.) How do
        // we know to properly relocate the `f` and `l` transforms?

        // Build a distribution for transforms aligned to the current token,
        // then remove any empty / null / undefined entries.
        let tokenDistribComponent = tokenDistribution.map((seq) => {
          const entry = seq.get(i);
          // Do not add empty Transforms into the correction-search input
          // at this stage.
          if(!entry || TransformUtils.isEmpty(entry.sample)) {
            return null;
          } else {
            return entry;
          }
        }).filter((entry) => !!entry);
        if(primaryInput) {
          let transformDistribution = tokenDistribComponent.length > 0 ? tokenDistribComponent : null;

          // If there are no entries in our would-be distribution, there's no
          // reason to pass in what amounts to a no-op.
          if(transformDistribution) {
            pushedToken.addSourceInput(primaryInput);
            // If we ever stop filtering tokenized transform distributions, it may
            // be worth adding an empty transform here with weight to balance
            // the distribution back to a cumulative prob sum of 1.
            pushedToken.searchSpace.addInput(transformDistribution);
          }
        } else if(incomingToken.text) {
          // We have no transform data to match against an inserted token with text; abort!
          // Refer to #12494 for an example case; we currently can't map previously-committed
          // input transforms to a newly split-off token.
          return null;
        }
        pushedToken.isWhitespace = incomingToken.isWhitespace;

        // Auto-replaces the search space to correspond with the new token.
        tokenization.push(pushedToken);
      }
    }

    return new ContextTokenization(tokenization, alignment);
  }
}

const appendText = (full: string, current: string) => full + current;
const prependText = (full: string, current: string) => current + full;

/**
 * Represents data about the token edited by an incoming Transform on the context's
 * edge that existed before the edit and remains within context after the edit -
 * the "edit boundary" token.
 */
interface EdgeEditBoundaryTokenData {
  /**
   * The text remaining in the token after the edit's deletions are applied,
   * before applying any inserts.
   */
  text: string;
  /**
   * The affected token's index in its source array.
   */
  tokenIndex: number;
  /**
   * Notes if the token was edited, thus potentially incomplete.
   */
  isPartial: boolean;
  /**
   * Notes if the boundary is ambiguous due to an empty token.
   *
   * The empty token may be immediately before the text insertion point or the
   * result of completely deleting a prior token.
   */
  omitsEmptyToken?: boolean
}

/**
 * Options that may be used to parameterize the range and scope of `buildEdgeWindow`.
 *
 * This is currently intended for use with unit-testing, allowing existing tests to
 * continue unimpeded even if we change the values for our related defined constants.
 */
interface EdgeWindowOptions {
  /**
   * Specifies a minimum number of unaffected Tokens to include within the edge window.
   */
  minTokens: number,

  /**
   * Specifies a minimum number of non-inserted characters to include within the edge window.
   */
  minChars: number
}

/**
 * Represents data about the context edge to which an incoming `Transform` will be applied.
 */
interface EdgeWindow {
  /**
   * The portion of text represented by the current tokenization that should be
   * made available for retokenization when applying the `Transform`.
   */
  retokenizationText: string,
  /**
   * Data about the token at the boundary of deleteLeft operations specified by the
   * `Transform`.
   */
  editBoundary: EdgeEditBoundaryTokenData,
  /**
   * Indicates the number of codepoints deleted per token encountered.
   *
   * The entries are ordered based on their distance from the specified edge, thus
   * will be in reverse index order when `applyAtFront == false`.
   */
  deleteLengths: number[],
  /**
   * Indicates the index that best defines the boundary for tokens included
   * within the edge window.
   *
   * When `applyAtFront == true`, the actual value is (last token index + 1).
   * - To retrieve tokens in the edge window:  `.slice(0,
   *   retokenizationEndIndex)
   * - To retrieve tokens not included: `.slice(retokenizationEndIndex);
   *
   * When `applyAtFront == false`, the actual value is (first token index).
   * - To retrieve tokens in the edge window: `.slice(retokenizationEndIndex)`
   * - To retrieve tokens not included: `.slice(0, retokenizationEndIndex)`.
   */
  sliceIndex: number
}

/**
 * Constructs a window on one side of the represented context that is aligned to
 * existing tokenization.
 * @param currentTokens  Tokens from an existing ContextTokenization
 * @param transform  A Transform specifying edits to apply to the represented
 * Context
 * @param applyAtFront  If true, applies the Transform and builds the window at
 * the start of the represented Context.
 *
 * If false, does both actions at the end
 * of the represented Context.
 * @returns
 */
export function buildEdgeWindow(
  currentTokens: ContextToken[],
  // Requires deleteRight be explicitly set.
  transform: Transform & { deleteRight: number },
  applyAtFront: boolean,
  windowOptions?: EdgeWindowOptions
): EdgeWindow {
  const totalDelete = applyAtFront ? transform.deleteRight : transform.deleteLeft;
  const directionSign = applyAtFront ? 1 : -1;

  // applyAtFront == true:   iterates forward
  // applyAtFront == false:  iterates backward
  const concatText: (full: string, current: string) => string = applyAtFront ? appendText : prependText;

  let retokenizationText = '';
  let deleteCnt = totalDelete;

  const deleteLengths: number[] = [];
  // Ensure any constructed edges (of the same deleteLeft value) for a given
  // tokenization always use the same window in their calculations.  May vary
  // for different deleteLeft values, but that's it.
  let tokenizeLength = 0;
  let tokenCount = 0;

  let editBoundary: EdgeEditBoundaryTokenData;

  let i: number;
  const endIndex = applyAtFront ? currentTokens.length - 1 : 0;
  const MIN_TOKENS = windowOptions?.minTokens ?? MIN_TOKENS_TO_RECONSIDER_FOR_TOKENIZATION;
  const MIN_CHARS  = windowOptions?.minChars ?? MIN_CHARS_TO_RECONSIDER_FOR_TOKENIZATION;
  for(
    i = applyAtFront ? 0 : currentTokens.length - 1;
    // Alternatively, applyAtFront ? i <= currentTokens.length - 1 : i >= 0;
    i != endIndex + directionSign; // stop @ 1 past
    i += directionSign
  ) {
    // Rather than have a monster end-condition in the loop's main definition,
    // we'll check these conditions here.  It seems more readable this way.
    if(tokenCount >= MIN_TOKENS && tokenizeLength >= MIN_CHARS) {
      break;
    }

    // True loop body - start
    const token = currentTokens[i].exampleInput;
    const tokenLen = KMWString.length(token);
    const tokenIsPartial = currentTokens[i].isPartial;

    // Do not consider partial tokens as part of the prefix area to use for tokenization.
    if(!tokenIsPartial) {
      tokenCount += (deleteCnt == 0) ? 1 : 0;
    }

    // if(editBoundaryToken) then all deletes have been applied.
    if(!editBoundary && totalDelete > 0) {
      const tokenDeleteLength = Math.min(deleteCnt, tokenLen);
      // Skip context-final empty tokens.
      if(tokenDeleteLength != 0 || deleteLengths.length != 0) {
        deleteLengths.push(tokenDeleteLength);
      }
      deleteCnt = Math.max(0, deleteCnt - tokenDeleteLength);
      // If the new remaining delete-count is zero, and we didn't delete a full
      // token, we hit the boundary; note the boundary text.
      if(deleteCnt == 0 && tokenDeleteLength != tokenLen) {
        editBoundary = {
          text: applyAtFront ? KMWString.slice(token, tokenDeleteLength) : KMWString.slice(token, 0, tokenLen - tokenDeleteLength),
          tokenIndex: i,
          isPartial: tokenDeleteLength != 0 || tokenIsPartial
        }
      }

      retokenizationText = concatText(retokenizationText, editBoundary?.text ?? '');
      tokenizeLength += tokenLen - tokenDeleteLength;
    } else {
      // if totalDeletes = 0, ensure we still construct an editBoundaryToken.
      if(!editBoundary) {
        editBoundary = {
          text: token,
          tokenIndex: i,
          isPartial: tokenIsPartial
        };
      }
      retokenizationText = concatText(retokenizationText, token);
      tokenizeLength += tokenLen;
    }
  }

  // Covers cases with full context deletion; the loop ends before this is built
  // for such cases.
  if(!editBoundary) {
    editBoundary = {
      text: '',
      tokenIndex: i - directionSign,
      isPartial: true
    }
  }

  // If the 'boundary token' is an empty token, shift it by one token - we might actually
  // be editing the token one index further.
  let shouldOmitEmptyToken = editBoundary.tokenIndex != 0 && editBoundary.tokenIndex == currentTokens.length - 1 && editBoundary.text == '';
  if(shouldOmitEmptyToken) {
    editBoundary = {
      text: currentTokens[editBoundary.tokenIndex-1].exampleInput,
      tokenIndex: editBoundary.tokenIndex + directionSign,
      isPartial: true
    }
  }

  // Second half of the condition:  handles cases with bad Transforms that try to overdelete.
  if(totalDelete == 0 || deleteCnt != 0) {
    deleteLengths.push(0);
  }

  editBoundary.omitsEmptyToken = shouldOmitEmptyToken;

  return {
    retokenizationText,
    editBoundary,
    deleteLengths,
    // Is used for slicing and should reflect the last token considered.
    // Forward:  the value indicates the interval's end (one past it, per typical indexing)
    // Backward:  the value indicates the interval's start (precisely, not past it)
    sliceIndex: i + (applyAtFront ? 0 : 1)
  }
}