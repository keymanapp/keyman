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
import { computeDistance } from './classical-calculation.js';
import { determineModelTokenizer } from '../model-helpers.js';
import { SegmentableDistanceCalculation } from './segmentable-calculation.js';

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

const MIN_TOKENS_TO_RECONSIDER_FOR_TOKENIZATION = 3;
const MIN_CHARS_TO_RECONSIDER_FOR_TOKENIZATION = 15;

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

  syncToSourceWindow(lexicalModel: LexicalModel, slideDeltas: {prefix: string, deleteCnt: number}): ContextTokenization {
    // Assertion: the current (sliding) context window is alignable and
    // valid deltas have been computed.

    // Step 1:  adjust current represented tokens based on actual context
    // window - all before applying any input transforms.
    const prefix = slideDeltas.prefix;
    let deleteCnt = slideDeltas.deleteCnt;

    if(prefix == '' && deleteCnt == 0) {
      return this;
    }

    const tokenize = determineModelTokenizer(lexicalModel);

    let tokenizationText = prefix;
    let tokenizeLength = KMWString.length(prefix);
    let tokenCount = 0;
    let tokenIndex = 0;

    let alteredToken: ContextToken;
    const currentTokens = this.tokens;
    for(
      let i = 0;
      i < currentTokens.length && (tokenCount < MIN_TOKENS_TO_RECONSIDER_FOR_TOKENIZATION
        || tokenizeLength < MIN_CHARS_TO_RECONSIDER_FOR_TOKENIZATION);
      i++
    ) {
      const token = currentTokens[i].exampleInput;
      const tokenLen = KMWString.length(token);

      // Do not consider partial tokens as part of the prefix area to use for tokenization.
      if(!currentTokens[i].isPartial) {
        tokenCount += (deleteCnt == 0) ? 1 : 0;
      }

      if(!alteredToken && slideDeltas.deleteCnt > 0) {
        if(!deleteCnt) {
          alteredToken = currentTokens[i];
          tokenizationText = token;
        } else if(deleteCnt < tokenLen) {
          tokenizationText = token.slice(deleteCnt);
          tokenizeLength += tokenLen - deleteCnt;
          alteredToken = new ContextToken(lexicalModel, tokenizationText, true);
        }
        deleteCnt = Math.max(0, deleteCnt - tokenLen);
      } else {
        tokenizationText += token;
      }
      tokenIndex++;
    }

    // These won't be altered; we're going to trust this tokenization is best,
    // as we determined it to be so in prior iterations.
    const preservedTokens = currentTokens.slice(tokenIndex);

    // If we deleted content, we can go ahead and end now; there's no need
    // to process any sort of prepended text that doesn't exist.
    if(slideDeltas.deleteCnt > 0) {
      if(alteredToken) {
        preservedTokens.unshift(alteredToken);
      }

      return new ContextTokenization(preservedTokens, null);
    }

    // If we made it here, we have new text at the context window's start. The
    // goal is to reasonably process it in case of large scale repeated
    // deletions.
    const retokenizing = currentTokens.slice(0, tokenIndex).map(t => t.exampleInput);

    // Step 2:  align this text and update tokens as needed.

    // Just take the best tokenization; it's not recent, so we'll have forgotten
    // fat-finger stuff here.
    //
    // Maaaaybe consider alternate tokenizations if reasonably likely for
    // epic/dict-breaker. But probably not.  Also, note #14753 in regard to
    // epic/dict-breaker for other notes of impact for this method.
    const initialTokenization = tokenize({ left: tokenizationText, startOfBuffer: false, endOfBuffer: false }).left.map(t => t.text);

    const calc = computeDistance(
      new SegmentableDistanceCalculation({diagonalWidth: retokenizing.length+2, noTransposes: true}),
      retokenizing,
      initialTokenization
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

      switch(op) {
        case 'match':
          currentTokens[input].isPartial = false;
          tokensToPrefix.push(currentTokens[input]);
          break;
        case 'insert':
        case 'substitute':
          tokensToPrefix.push(new ContextToken(lexicalModel, initialTokenization[match], i == 0));
        default:
          // do nothing.
      }
    }

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