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
import { computeDistance, EditOperation, EditTuple } from './classical-calculation.js';
import { determineModelTokenizer } from '../model-helpers.js';
import { ExtendedEditOperation, SegmentableDistanceCalculation } from './segmentable-calculation.js';

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

// May be able to "get away" with 2 & 5 or so, but having extra will likely help
// with edit path stability.
const MIN_TOKENS_TO_RECONSIDER_FOR_TOKENIZATION = 3;
const MIN_CHARS_TO_RECONSIDER_FOR_TOKENIZATION = 8;

interface EditTokenMap {
  index: number,
  text: string
};

interface TokenMergeMap {
  inputs: EditTokenMap[],
  match: EditTokenMap
};

interface TokenSplitMap {
  input: EditTokenMap,
  matches: (EditTokenMap & { textOffset: number })[]
};

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
   *
   * Intended for debugging use only.
   */
  get sourceText() {
    return this.tokens.map(token => token.sourceText);
  }

  /**
   * Returns a plain-text string representing the most probable representation for all
   * tokens represented by this tokenization instance.
   */
  get exampleInput(): string[] {
    return this.tokens.map(token => token.exampleInput);
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
    return computeAlignment(this.exampleInput, incomingTokenization, isSliding, noSubVerify);
  }

  /**
   * Applies the specified Transform to the _left-hand_ side of the context in
   * order to update and match the current contents of the sliding context
   * window.
   *
   * Cached tokenizations reflect the state of context after the most recent
   * edit but before any subsequent context-window slide.  This operation should
   * return a tokenization matching the state _after_ the subsequent slide,
   * which is provided on subsequent inputs based on the resulting context
   * state.
   * @param lexicalModel The active lexical model
   * @param transform Specifies _either_ an `.insert` string to extend the
   * context (if the last edit deleted chars) _or_ a `.deleteLeft` (to shrink
   * the context if the last edit added chars).
   * @returns
   */
  applyContextSlide(lexicalModel: LexicalModel, transform: Transform & { deleteRight: number }): ContextTokenization {
    // Assumption: the current (sliding) context window is alignable and valid
    // deltas have been computed.

    // Assumption:  the transform will EITHER have an insert OR a deleteRight.
    // Not both.

    // Assumption:  deleteLeft is always empty.  (There's nothing to the left;
    // we apply on that side.)
    if(TransformUtils.isEmpty(transform)) {
      // No edits needed?  Why retokenize?
      return new ContextTokenization(this);
    }

    // Step 1: build a window for window-start retokenization in case the context-window slid.
    const currentTokens = this.tokens;
    const {
      retokenizationText,
      editBoundary,
      sliceIndex: edgeSliceIndex
    } = buildEdgeWindow(currentTokens, transform, true);

    // If we deleted content, we can go ahead and end now; there's no need
    // to process any sort of prepended text that doesn't exist.
    if(transform.deleteRight > 0) {
      // Slice off the token at the boundary and all those left of it.
      const preservedTokens = currentTokens.slice(editBoundary.tokenIndex+1);
      // If the token at the boundary had remaining text, rebuild a simple
      // token for it, but don't try to preserve its fat-finger data any more.
      // (To do so may be a complex problem for little return.)
      if(editBoundary.text) {
        preservedTokens.unshift(new ContextToken(lexicalModel, editBoundary.text, editBoundary.isPartial));
      }

      // And done.  Why retokenize?  We already had a proper tokenization;
      // preserve any boundaries that remain within the context window.
      return new ContextTokenization(preservedTokens, null);
    }

    // Step 2:  adjust current represented tokens based on actual context
    // window - all before applying any input transforms.

    // If we made it here, we have new text at the context window's start. The
    // goal is to reasonably process it in case of large scale repeated
    // deletions.
    const sectionToRetokenize = currentTokens.slice(0, edgeSliceIndex).map(t => t.exampleInput);
    // Avoid having the edit path merge empty tail tokens to their predecessor.
    if(sectionToRetokenize[sectionToRetokenize.length - 1] == '') {
      sectionToRetokenize.pop();
    }

    // Step 3:  align this text and update tokens as needed.

    // Just take the best tokenization; it slid out of view and no longer has
    // associated fat-finger keystroke data.
    //
    // Maaaaybe consider alternate tokenizations if reasonably likely for
    // epic/dict-breaker. But probably not.  Also, note #14753 in regard to
    // epic/dict-breaker for other notes of impact for this method.
    const tokenize = determineModelTokenizer(lexicalModel);
    const slidAndRetokenized = tokenize({ left: transform.insert + retokenizationText, startOfBuffer: false, endOfBuffer: false }).left.map(t => t.text);

    const calc = computeDistance(
      new SegmentableDistanceCalculation({diagonalWidth: sectionToRetokenize.length+2, noTransposes: true}),
      sectionToRetokenize,
      slidAndRetokenized
    );

    // Rebuild early tokenization from this.
    const editPaths = calc.editPath();

    // We don't expect anything but a match for the last token; it had been stable.
    // We'll accept a 'substitute' if necessary over other edits - splits and merges
    // should only happen on the sliding context window's boundary.

    // 'match':  ideal
    const editPath = editPaths.find((path) => path[path.length-1].op == 'match')
    // 'substitute':  may happen if best suggestions for a token don't actually
    // match the original context.  May happen on occasion.
      || editPaths.find((path) => path[path.length-1].op == 'substitute')
    // failsafe - completely reconstruct from scratch if needed.
      || editPaths[0];

    const tokensToPrefix: ContextToken[] = [];
    for(let i = 0; i < editPath.length; i++) {
      const { op, input, match } = editPath[i];
      let mergeOffset = 0;

      switch(op) {
        case 'match':
          currentTokens[input].isPartial = false;
          tokensToPrefix.push(currentTokens[input]);
          break;
        case 'merge':
          const mergeTarget = slidAndRetokenized[match];
          let text = currentTokens[input].exampleInput;
          // Consume all but the last component of the merge; it shouldn't be
          // duplicated in the resulting tokenization.
          mergeOffset = 1;
          let nextText = text + currentTokens[input + mergeOffset].exampleInput;
          for(; mergeTarget.indexOf(nextText) != -1; nextText = text + currentTokens[input + 1 + mergeOffset++]) {
            i++;
          }
          // We incremented one too many times; adjust the value back down.
          mergeOffset--;
          // fallthrough; // the common `tokensToPrefix.push()` applies here just as well.
        case 'split':
          // We'll keep it simple here; we probably lack fat-finger data, so
          // just rebuild each from scratch.  We could do something more with
          // merge (if _later_ components still have fat-finger data), but it's
          // likely not worth the trouble.

          // Also, each split entry is a valid token - it's the source token
          // that'd show up multiple times in the stream, unlike with 'merge'.

          // fallthrough;
        case 'insert':
        case 'substitute':
          tokensToPrefix.push(new ContextToken(lexicalModel, slidAndRetokenized[match], i - mergeOffset == 0));
          break;
        default:
          // do nothing.
      }
    }

    // These won't be altered; we're going to trust this tokenization is best,
    // as we determined it to be so in prior iterations.
    const preservedTokens = currentTokens.slice(edgeSliceIndex);

    // TODO:  determine alignment values to store, if any, re: context alignment.
    // ... maybe allow saving the slide transform as its own thing?
    return new ContextTokenization(tokensToPrefix.concat(preservedTokens), null);
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
      if(this.tokens[matchingOffset+i].exampleInput != tokenizedContext[incomingOffset+i].text) {
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
    let primaryInputAppliedLen = 0;
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
        const dist = tokenDistribution.map((seq) => seq.get(tailIndex) ?? emptySample);
        token.addInput({trueTransform: primaryInput ?? emptySample.sample, inputStartIndex: primaryInputAppliedLen}, dist);
      }

      tokenization[incomingIndex] = token;
      primaryInputAppliedLen += KMWString.length(primaryInput?.insert ?? '');
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
            // If we ever stop filtering tokenized transform distributions, it may
            // be worth adding an empty transform here with weight to balance
            // the distribution back to a cumulative prob sum of 1.
            pushedToken.addInput({ trueTransform: primaryInput, inputStartIndex: primaryInputAppliedLen }, transformDistribution);
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
        primaryInputAppliedLen += KMWString.length(primaryInput.insert);
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
          text: applyAtFront ? KMWString.substring(token, tokenDeleteLength) : KMWString.substring(token, 0, tokenLen - tokenDeleteLength),
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

/**
 * Using the post-application tokenization of a context, traces where the
 * components of a transform land among its tokens and computes relevant
 * metadata for tokenization-transition analysis.
 * @param tokens The tokenized form of the context after being edited by the
 * transform
 * @param transform  The edit applied to a context
 * @returns
 */
export function traceInsertEdits(tokens: string[], transform: Transform): {
  /**
   * a tokenization of the Transform's `insert` string, in stack form.
   */
  stackedInserts: string[],
  /**
   * The index of the earliest post-application token to which the `Transform`'s
   * `.insert` component was applied.
   */
  firstInsertPostIndex: number
} {
  let insert = transform.insert;
  let insertLen = KMWString.length(insert);
  const stackedInserts: string[] = [];
  let firstInsertPostIndex: number;

  if(insert.length > 0) {
    for(let index = tokens.length - 1; index >= 0; index--) {
      const tokenLen = KMWString.length(tokens[index]);

      const currentToken = tokens[index];
      if(tokenLen >= insertLen) {
        stackedInserts.push(insert);
        firstInsertPostIndex = index;
        break;
      }

      insert = KMWString.substring(insert, 0, insertLen - tokenLen);
      insertLen -= tokenLen;
      stackedInserts.push(currentToken);
      firstInsertPostIndex = index;
    }
  } else {
    firstInsertPostIndex = tokens.length - 1
    stackedInserts.push('');
  }

  return {
    stackedInserts,
    firstInsertPostIndex
  };
}

/**
 * Given context tokenizations before and after applying an incoming transform,
 * this method analyzes the tokenizations in order to make adjustments for any
 * 'merge' or 'split' edits needed for the transition.
 * @param priorTokenization
 * @param resultTokenization
 * @returns
 */
export function analyzePathMergesAndSplits(priorTokenization: string[], resultTokenization: string[]): {
  /**
   * Notes the shift in pre-edit token index needed to map the tail token index
   * to its post-edit index due to 'merge' edits.
   */
  mergeOffset: number,
  /**
   * Notes the shift in post-edit token index needed to map the tail token index
   * to its pre-edit index due to 'split' edits.
   */
  splitOffset: number,
  /**
   * The edit operations needed to transition from the first (prior)
   * tokenization to the second (result) tokenization.
   */
  editPath: EditTuple<ExtendedEditOperation>[],
  /**
   * The edit operations needed _after_ applying any merge or split operations
   * to the first (prior) tokenization in order to transition to the second
   * (result) tokenization.
   *
   * No 'merge' or 'split' edits will appear in this version.
   */
  mappedPath: EditTuple<EditOperation>[],
  /**
   * Indicates groupings of directly related merges.  Without loss of
   * generality, if two separate groups of tokens are merged, two groups will be
   * defined - one for each token resulting from a merge.
   */
  merges: TokenMergeMap[],
  /**
   * Indicates groupings of directly related splits.  Without loss of
   * generality, if two separate tokens are split, two groups will be defined -
   * one for each source token split.
   */
  splits: TokenSplitMap[]
} {
  // We've found the root token to which changes may apply.
  // We've found the last post-application token to which transform changes contributed.
  // Did anything shift at or near that intersection?
  const preTokenization = priorTokenization;
  const calc = computeDistance(
    new SegmentableDistanceCalculation({
      diagonalWidth: Math.abs(preTokenization.length - resultTokenization.length) + 2,
      noTransposes: true
    }),
    preTokenization,
    resultTokenization
  );

  const editPath = calc.editPath()[0];

  /*
    * This next major block will check the edit path for splits and merges,
    * producing a non-'extended' path that results from their application.
    *
    * Transform indexing is based upon the non-'extended' style results, and it
    * can be important to determine whether the lead transform applies to the
    * result token for either edit type, or to a token immediately after the
    * result.
    *
    * Preconditions:
    * - not used with epic/dict-breaker
    * - per unicode wordbreaker, no massive shifting of word boundaries for
    *   prior text.
    *   - we can't handle part of one token being split off and simultaneously
    *     merged to another.
    */

  let queueIndex = 0;
  const mappedPath: EditTuple<EditOperation>[] = [];
  let mergeOffset = 0;
  let splitOffset = 0;
  const merges: TokenMergeMap[] = [];
  const splits: TokenSplitMap[] = [];
  while(queueIndex < editPath.length) {
    const edit = editPath[queueIndex];
    const { input, match } = edit;
    let op = edit.op;

    let inputOffset: number = 0;
    let matchOffset: number = 0;
    if(op == 'merge') {
      const mergeTarget = resultTokenization[match];
      const merge: TokenMergeMap = {
        match: {
          index: match,
          text: mergeTarget
        },
        inputs: [ {
          index: input,
          text: preTokenization[input]
        }]
      };
      let currentMerge = preTokenization[input];
      let inputLookahead = 1;
      // Look-ahead 1
      let nextMerge = currentMerge + preTokenization[input + inputLookahead++];
      // Conditional validates if look-ahead 1 passes (which it should)
      for(/* next line */; mergeTarget.indexOf(nextMerge) == 0; nextMerge = currentMerge + preTokenization[input + inputLookahead++]) {
        merge.inputs.push({
          index: input + inputLookahead - 1,
          text: preTokenization[input + inputLookahead-1]
        });
        currentMerge = nextMerge;
        // Each time we 'pass' the condition, we've successfully processed an associated edit.
        queueIndex++;
        mergeOffset--;
        inputOffset++;
      }

      op = currentMerge == mergeTarget ? 'match' : 'substitute';
      merges.push(merge);
    } else if(op == 'split') {
      const splitTarget = preTokenization[input];
      const split: TokenSplitMap = {
        input: {
          index: input,
          text: splitTarget
        },
        matches: [ {
          index: match,
          text: resultTokenization[match],
          textOffset: 0
        }],
      };
      let currentMerge = resultTokenization[match];
      matchOffset = 1;
      // Look-ahead 1
      let nextMerge = currentMerge + resultTokenization[match + matchOffset++];
      for(/* next line */; splitTarget.indexOf(nextMerge) == 0; nextMerge = currentMerge + preTokenization[match + matchOffset++]) {
        const textOffset = KMWString.length(currentMerge);
        currentMerge = nextMerge;
        split.matches.push({
          index: match + matchOffset - 1,
          text: resultTokenization[match + matchOffset-1],
          textOffset
        });
        // Each time we 'pass' the condition, we've successfully processed an associated edit.

        // Add the token before the recently adjoined one.
        mappedPath.push({op: 'match', input: input + mergeOffset - splitOffset, match: match + matchOffset - 2});
        queueIndex++;
        splitOffset--;
      }

      // Set up for the last split-off token.
      op = currentMerge == splitTarget ? 'match' : 'substitute';
      matchOffset -= 2;
      splits.push(split);
    }

    let reprocEdit: Partial<EditTuple<EditOperation>> = { op };
    if(input !== undefined) {
      reprocEdit.input = input + mergeOffset - splitOffset + inputOffset;
    }
    if(match !== undefined) {
      reprocEdit.match = match + matchOffset;
    }
    mappedPath.push(reprocEdit as EditTuple<EditOperation>);

    queueIndex++;
  }

  return { mergeOffset, splitOffset, editPath, mappedPath, merges, splits };
}