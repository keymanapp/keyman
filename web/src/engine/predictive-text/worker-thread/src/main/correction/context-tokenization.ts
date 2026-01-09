/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about one potential tokenization of contents of
 * the sliding context window for one specific instance of context state.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';
import { KMWString } from '@keymanapp/web-utils';

import { ContextToken } from './context-token.js';
import TransformUtils from '../transformUtils.js';
import { computeDistance, EditOperation, EditTuple } from './classical-calculation.js';
import { determineModelTokenizer } from '../model-helpers.js';
import { ExtendedEditOperation, SegmentableDistanceCalculation } from './segmentable-calculation.js';
import { PendingTokenization } from './tokenization-subsets.js';

import LexicalModel = LexicalModelTypes.LexicalModel;
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

export interface TokenSplitMap {
  input: EditTokenMap,
  matches: (EditTokenMap & { textOffset: number })[]
};

/**
 * Represents the portion of a tokenization that may be affected by incoming
 * Transforms and how the existing components of the tokenization are affected
 * by any word-boundary shifts in existing content that occur due incoming
 * Transform side-effects.
 */
export interface TokenizationEdgeAlignment {
  /**
   * Denotes any token merge edits needed after applying the Transform.
   */
  merges: TokenMergeMap[];
  /**
   * Denotes any token split edits needed after applying the Transform.
   */
  splits: TokenSplitMap[];
  /**
   * Denotes any further token edits needed that cannot be attributed to
   * 'merge's, 'split's, or edits from the input `Transform`.
   */
  unmappedEdits: EditTuple<EditOperation>[];

  /**
   * The edge window defining the potential range of tokenization adjustments
   * needed after applying the Transform
   */
  edgeWindow: RetokenizedEdgeWindow;

  /**
   * The number of tokens deleted from the end, without being erased.
   */
  removedTokenCount: number;
}

/**
 * An analysis on how an existing tokenization and its contents will be affected
 * by applying an input `Transform`.
 */
export interface TokenizationTransitionEdits {
  /**
   * Represents the portion of the existing tokenization that may be affected by
   * the incoming Transform.
   *
   * Also represents the changes needed to model any word-boundary shifts
   * implied by the Transform but not generated within new content produced by
   * its text edits.
   */
  alignment: TokenizationEdgeAlignment;

  /**
   * The tokenized form of the input Transform, indexed by position relative to
   * the end of the original context's tail token.
   */
  tokenizedTransform: Map<number, Transform>;
}

/**
 * This class represents the sequence of tokens (words and whitespace blocks)
 * held within the active sliding context-window at a single point in time.
 */
export class ContextTokenization {
  /**
   * The sequence of tokens in the context represented by this instance.
   */
  readonly tokens: ContextToken[];
  /**
   * The tokenization-transition metadata relating this instance to the most likely
   * tokenization from a prior state.
   */
  readonly transitionEdits?: PendingTokenization;

  /**
   * The portion of edits from the true input keystroke that are not part of the
   * final entry in `token`.  If `null`, all edits are considered part of the
   * final token's contents.
   *
   * If the final token is new due to a newly-introduced wordboundary traversed
   * by the keystroke, this will generally be set to an empty transform that
   * 'finalizes' the previous tail token.
   *
   * (Refer to #12494 for an example case.)
   */
  readonly taillessTrueKeystroke: Transform;

  constructor(priorToClone: ContextTokenization);
  constructor(tokens: ContextToken[]);
  constructor(tokens: ContextToken[], alignment: PendingTokenization, taillessTrueKeystroke: Transform);
  constructor(
    param1: ContextToken[] | ContextTokenization,
    alignment?: PendingTokenization,
    taillessTrueKeystroke?: Transform
  ) {
    if(!(param1 instanceof ContextTokenization)) {
      const tokens = param1;
      this.tokens = [].concat(tokens);
      this.transitionEdits = alignment;
      this.taillessTrueKeystroke = taillessTrueKeystroke;
    } else {
      const priorToClone = param1;
      this.tokens = priorToClone.tokens.map((entry) => new ContextToken(entry));
      this.transitionEdits = priorToClone.transitionEdits ? {...priorToClone.transitionEdits} : null;
      this.taillessTrueKeystroke = priorToClone.taillessTrueKeystroke;
    }
  }

  /**
   * Returns the token adjacent to the text insertion point.
   */
  get tail(): ContextToken {
    return this.tokens[this.tokens.length - 1];
  }

  /**
   * Returns the unique correction-search space identifier corresponding to this tokenization.
   */
  get spaceId(): number {
    return this.tail.spaceId;
  }

  /**
   * Returns a plain-text string representing the most probable representation for all
   * tokens represented by this tokenization instance.
   */
  get exampleInput(): string[] {
    return this.tokens.map(token => token.exampleInput);
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
      return new ContextTokenization(preservedTokens);
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

    return new ContextTokenization(tokensToPrefix.concat(preservedTokens));
  }

  /**
   * Given the existing tokenization and an incoming input `Transform`, this
   * method precomputes how both the current, pre-application tokenization will
   * be altered and how the incoming Transform will be tokenized.
   *
   * Note that this method is designed for use with languages that employ
   * classical space-based wordbreaking.  Do not use it for languages that need
   * dictionary-based wordbreaking support!
   * @param lexicalModel
   * @param transform
   * @param edgeOptions
   * @returns
   */
  mapWhitespacedTokenization(
    lexicalModel: LexicalModel,
    transform: Transform,
    edgeOptions?: EdgeWindowOptions
  ): TokenizationTransitionEdits {
    // Step 4:  now that our window's been properly updated, determine what the
    // input's effects on the context is.
    //
    // Context does not slide within this function.
    //
    // Assumption:  this alignment cannot fail; we KNOW there's a solid
    // before-and-after relationship here, and we can base it on the results of
    // a prior syncToSourceWindow call.
    //
    // We don't wish to do the full tokenization here - we only want to check
    // over the last few tokens that might reasonably shift.  We also want to
    // batch effects.

    // Do not mutate the original transform; it can cause unexpected assertion
    // effects in unit tests.
    const edgeTransform = {...transform, deleteRight: transform.deleteRight || 0};
    const edgeWindow = buildEdgeWindow(this.tokens, edgeTransform, false, edgeOptions);
    const {
      retokenizationText,
      editBoundary,
      sliceIndex: edgeSliceIndex
    } = edgeWindow;
    // Prevent mutation of the original return property.
    const stackedDeletes = edgeWindow.deleteLengths.slice();

    const tokenize = determineModelTokenizer(lexicalModel);
    const postTokenization = tokenize({left: retokenizationText + transform.insert, startOfBuffer: true, endOfBuffer: true}).left.map(t => t.text);
    if(postTokenization.length == 0) {
      postTokenization.push('');
    }
    const { stackedInserts, firstInsertPostIndex } = traceInsertEdits(postTokenization, transform);

    // What does the edge's retokenization look like when we remove the inserted portions?
    const retokenizedEdge = postTokenization.slice(0, firstInsertPostIndex);
    const insertBoundaryToken = postTokenization[firstInsertPostIndex];

    // Note:  requires that helpers have not mutated `stackedInserts`.
    const uninsertedBoundaryToken = KMWString.substring(insertBoundaryToken, 0, KMWString.lastIndexOf(insertBoundaryToken, stackedInserts[0]));

    // Do not preserve empty tokens here, even if tokenization normally would produce one.
    // It's redundant and replaceable for tokenization batching efforts.
    if(uninsertedBoundaryToken != '') {
      retokenizedEdge.push(uninsertedBoundaryToken);
    }

    // We've found the root token within the root context state to which deletes (and inserts)
    // may be applied.
    // We've also found the last post-application token to which transform changes contributed.
    // How do these indices line up - we need to properly construct and index our transforms,
    // but 'merge' and 'split' edits can mess up that indexing.

    const currentTokens = this.tokens;
    const preTokenization = currentTokens
      .slice(edgeSliceIndex, editBoundary.tokenIndex+1)
      .map(t => t.exampleInput);

    // Determine the effects of splits & merges as applied to the original
    // cached context state.
    const { mergeOffset, splitOffset, editPath, merges, splits } = analyzePathMergesAndSplits(
      preTokenization,
      postTokenization.slice(0, firstInsertPostIndex+1)
    );

    /*
     * Final steps:  We can now safely index the transforms.  Let's do it!
     * 1. Determine the first index a Transform may align to
     * 2. Build the transforms
     *
     * Notes:
     * - text applied to the end of a 'merged' token at the tail:  should have
     *   index 0, not -1.
     *   - pretokenization index will mismatch by -1: -SUM(merge size - 1)
     *   - Ex: can + ' + t => can't
     *          -1   0          0
     * - text applied to the end of a 'split' token at the tail:  should also
     *   have index 0, not 1.
     *   - posttokenization index will mismatch by +1: SUM(split size - 1)
     *   - new token after 'split':  index 1
     *   - Ex: can' + ? => can + ' + ?
     *          0          -1    0   1
     *
     * The first transform applies at the end of the retokenized zone and its
     * associated index.  The question:  were there deletes that occurred?
     */

    const lastEditedPreTokenIndex = editBoundary.tokenIndex - edgeSliceIndex;
    let shiftDeletes = false;
    // first popped entry == 0 - a delete no-op.
    if(stackedDeletes[stackedDeletes.length - 1] == 0) {
      // the boundary indices found by both methods above differ
      if(lastEditedPreTokenIndex + mergeOffset != firstInsertPostIndex + splitOffset) {
        shiftDeletes = true;
      }

      // there are no inserts, so we don't affect the boundary token we landed on.
      if(stackedDeletes.length > 1 && transform.insert == '') {
        shiftDeletes = true;
      }
    }

    if(shiftDeletes) {
      // Do not add a zero-length delete if we're not actually altering the
      // corresponding token at all.
      stackedDeletes.pop();
    }

    // The first delete always applies to index 0. If the built edge window
    // omits a context-final empty-string, adjust the tokenization indices
    // accordingly.
    const tailIndex = 0 - (stackedDeletes.length - 1) + (editBoundary.omitsEmptyToken ? -1 : 0);
    // Mutates stackedInserts, stackedDeletes.
    const baseRemovedTokenCount = Math.max(0, stackedDeletes.length - stackedInserts.length);
    const transformMap = assembleTransforms(stackedInserts, stackedDeletes, tailIndex);

    // If there's an empty transform in the 0 position and we already know we're
    // dropping tokens - and only deleting - we're dropping an
    // otherwise-untracked empty token - make sure it's included!
    const droppedFinalTransform = baseRemovedTokenCount > 0 && transform.insert == '' && TransformUtils.isEmpty(transformMap.get(0));
    // Past that, if we have more delete entries than insert entries for our transforms, we
    // dropped some tokens outright.
    const removedTokenCount = baseRemovedTokenCount + (droppedFinalTransform ? 1 : 0);

    // Final step:  check for any unexpected boundary shifts not mappable to 'merge' / 'split'
    // and not caused by transforms.  All transforms always apply in sequence at the end.
    const unmappedEdits: EditTuple<EditOperation>[] = [];
    for(let i = 0; i < editPath.length - transformMap.size; i++) {
      const op = editPath[i].op;
      switch(op) {
        case 'merge':
        case 'split':
          // already calculated
          // can fall through to the `continue;` line.
        case 'match':
          continue;
        default:
          // Should only be substitutions here.
          // We may wish to add extra analysis in the future when supporting
          // prediction from multiple competing tokenizations.
          unmappedEdits.push(editPath[i] as EditTuple<EditOperation>);
      }
    }

    return {
      alignment: {
        edgeWindow: {...edgeWindow, retokenization: retokenizedEdge},
        merges,
        splits,
        unmappedEdits,
        removedTokenCount
      },
      tokenizedTransform: transformMap,
    };
  }

  /**
   * Given results from `precomputeTokenizationAfterInput`, this method will
   * evaluate the pending transition in tokenization for all associated inputs
   * while reusing as many correction-search intermediate results as possible.
   * @param pendingTokenization Batched results from one or more
   * `precomputeTokenizationAfterInput` calls on this instance, all with the
   * same alignment values.
   * @param lexicalModel The active lexical model
   * @param sourceInput The Transform associated with the keystroke triggering
   * the transition.
   * @param bestProbFromSet The probability of the single most likely input
   * transform in the overall transformDistribution associated with the
   * keystroke triggering the transition.  It need not be represented by the
   * pendingTokenization to be built.
   * @returns
   */
  evaluateTransition(
    pendingTokenization: PendingTokenization,
    lexicalModel: LexicalModel,
    sourceInput: Transform,
    bestProbFromSet: number
  ): ContextTokenization {
    const { alignment: alignment, inputs } = pendingTokenization;
    const sliceIndex = alignment.edgeWindow.sliceIndex;
    const baseTokenization = this.tokens.slice(sliceIndex);
    let affectedToken: ContextToken;

    const tokenization: ContextToken[] = [];

    // Assumption:  all three are in sorted index order.  (They're created that way.)
    const { merges, splits, unmappedEdits } = alignment;
    // Handle merges, splits, unmapped edits.

    for(let i = 0; i < baseTokenization.length; i++) {
      if(merges[0]?.inputs[0].index == i) {
        // do a merge!  Also, note that we've matched the first index of the merge.
        // consider:  move to ContextToken as class method.  (static?)
        const merge = merges.shift();
        const tokensToMerge = merge.inputs.map((m) => baseTokenization[m.index]);
        const mergeResult = ContextToken.merge(tokensToMerge, lexicalModel);
        tokenization.push(mergeResult);
        i = merge.inputs[merge.inputs.length - 1].index;
        continue;
      }

      if(splits[0]?.input.index == i) {
        // do a split!
        const split = splits.shift();
        const splitResults = baseTokenization[i].split(split, lexicalModel);
        const resultStack = splitResults.reverse();
        while(resultStack.length > 0) {
          tokenization.push(resultStack.pop());
        }
        continue;
      }

      if(unmappedEdits[0]?.input == i) {
        // fix things up
        throw new Error("Not yet supported.");
      }

      const token = new ContextToken(baseTokenization[i]);
      token.isPartial = false;
      tokenization.push(token);
    }

    // Assumption:  inputs.length > 0.  (There is at least one input transform.)
    const inputTransformKeys = [...inputs[0].sample.keys()];
    let removedTokenCount = alignment.removedTokenCount;
    while(removedTokenCount-- > 0) {
      inputTransformKeys.pop();
      tokenization.pop();
    }

    let appliedLength = 0;
    for(let tailRelativeIndex of inputTransformKeys) {
      let distribution = inputs.map((i) => ({sample: i.sample.get(tailRelativeIndex), p: i.p}));
      const tokenIndex = (tokenization.length - 1) + tailRelativeIndex;

      affectedToken = tokenization[tokenIndex];
      if(!affectedToken) {
        affectedToken = new ContextToken(lexicalModel);
        tokenization.push(affectedToken);
      } else if(KMWString.length(affectedToken.exampleInput) == distribution[0].sample.deleteLeft) {
        // If the entire token will be replaced, throw out the old one and start anew.
        affectedToken = new ContextToken(lexicalModel);
        // Replace the token at the affected index with a brand-new token.
        tokenization.splice(tokenIndex, 1, affectedToken);
      }

      affectedToken.isPartial = true;
      delete affectedToken.appliedTransitionId;

      // If we are completely replacing a token via delete left, erase the deleteLeft;
      // that part applied to a _previous_ token that no longer exists.
      // We start at index 0 in the insert string for the "new" token.
      if(affectedToken.inputCount == 0 && distribution[0].sample.deleteLeft != 0) {
        distribution = distribution.map((mass) => ({sample: { ...mass.sample, deleteLeft: 0 }, p: mass.p }));
      }
      affectedToken.addInput({
        segment: {
          trueTransform: sourceInput,
          transitionId: sourceInput.id,
          start: appliedLength
        },
        bestProbFromSet: bestProbFromSet
      }, distribution);
      appliedLength += KMWString.length(distribution[0].sample.insert);

      const tokenize = determineModelTokenizer(lexicalModel);
      affectedToken.isWhitespace = tokenize({left: affectedToken.exampleInput, startOfBuffer: false, endOfBuffer: false}).left[0]?.isWhitespace ?? false;

      affectedToken = null;
    }

    return new ContextTokenization(
      this.tokens.slice(0, sliceIndex).concat(tokenization),
      null /* tokenMapping */,
      determineTaillessTrueKeystroke(pendingTokenization)
    );
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
   * Indicates the ContextToken sourceRangeKey corresponding to the boundary token.
   */
  sourceRangeKey: string;
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
 * Represents the context edge to which an incoming `Transform` will be applied
 * and how it is retokenized given the `Transform` as input.
 */
interface RetokenizedEdgeWindow extends EdgeWindow {
  /**
   * The new tokenization (and its implied boundaries) for the edge window's
   * represented text.
   */
  retokenization: string[];
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
      deleteLengths.push(tokenDeleteLength);
      deleteCnt = Math.max(0, deleteCnt - tokenDeleteLength);
      // If the new remaining delete-count is zero, and we didn't delete a full
      // token, we hit the boundary; note the boundary text.
      if(deleteCnt == 0 && tokenDeleteLength != tokenLen) {
        editBoundary = {
          sourceRangeKey: currentTokens[i].sourceRangeKey,
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
          sourceRangeKey: currentTokens[i].sourceRangeKey,
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
      sourceRangeKey: currentTokens[0].sourceRangeKey,
      text: '',
      tokenIndex: i - directionSign,
      isPartial: true
    }
  }

  // If the 'boundary token' is an empty token, shift it by one token - we might actually
  // be editing the token one index further.
  let shouldOmitEmptyToken = editBoundary.tokenIndex != 0 && editBoundary.tokenIndex == currentTokens.length - 1 && editBoundary.text == '';
  if(shouldOmitEmptyToken) {
    const effectiveTail = currentTokens[editBoundary.tokenIndex-1];
    editBoundary = {
      sourceRangeKey: effectiveTail.sourceRangeKey,
      text: effectiveTail.exampleInput,
      tokenIndex: editBoundary.tokenIndex + directionSign,
      isPartial: true
    }
  }

  // If there was no specified delete length, still emit a single delete 0.
  if(totalDelete == 0) {
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

/**
 * Constructs tokenized Transforms based on the results of prior analysis steps
 * starting from the specified index.
 * @param stackedInserts A stack of strings tokenized from the original applied
 * Transform's `insert` string.
 * @param stackedDeletes A stack of integers tokenized from the original applied
 * Transform's `delete` string.
 * @param tailIndex The index of the post-application tail token relative to its
 * aligned pre-application index.
 * @returns A map of tokenized Transforms and their associated indices relative
 * to the position of the original tail token and where it would align within
 * the post-application context.
 */
export function assembleTransforms(stackedInserts: string[], stackedDeletes: number[], tailIndex: number) {
  const transformMap: Map<number, Transform> = new Map();
  // 'stacked' - from late in context to early in context.
  while(stackedInserts.length || stackedDeletes.length) {
    // delete lefts should be set on the front-most inserts!
    const deleteLeft = stackedDeletes.pop() ?? 0;
    const insert = stackedInserts.pop() ?? '';
    // If both are empty at the start, don't add an empty transform. Delete
    // detection may have overshot and added an entry for a word we shouldn't
    // edit (that is not at this index!).  They're fine later in the transform list, though.
    if(transformMap.size != 0 || insert || deleteLeft) {
      transformMap.set(tailIndex++, { insert, deleteLeft });
    } // else bypass - without incrementing the index!
  }

  if(!transformMap.size) {
    transformMap.set(tailIndex++, {insert: '', deleteLeft: 0});
  }

  return transformMap;
}

/**
 * Used to construct and represent the part of the incoming transform that does
 * not land as part of the final token in the resulting context.  This component
 * should be preserved by any suggestions that get applied.
 * @param tokenizationAnalysis
 * @returns
 */
export function determineTaillessTrueKeystroke(tokenizationAnalysis: PendingTokenization) {
  // undefined by default; we haven't yet determined if we're still affecting
  // the same token that was the tail in the previous tokenization state.
  let taillessTrueKeystroke: Transform;

  // If tokens were inserted, emit an empty transform; this prevents
  // suggestions from replacing the "current" token.
  const bestTokenizedInput = tokenizationAnalysis.inputs[0].sample;
  if(bestTokenizedInput.has(1)) {
    // Sets a default transform that will be returned even if the main
    // transform body lies entirely within a new token.
    taillessTrueKeystroke = { insert: '', deleteLeft: 0 };

    // While the .size() > 1 case could also land here, it is ALSO covered
    // by the loop that follows, without fail.
  }

  const transformKeys = [...tokenizationAnalysis.inputs[0].sample.keys()];
  transformKeys.pop();

  for(let i of transformKeys) {
    /*
      * Thinking ahead to multitokenization:
      *
      * If what we have is not on the "true" tokenization, then... we need to
      * do multitoken effects, right?  We're basing new suggestions based on a
      * state that does not currently exist!  We'd need to enforce THAT state,
      * *then* do the suggestion!
      * - Which gets fun if we auto-apply such a case, as the new "true" tokenization
      *   no longer results directly from the true input.
      *
      * If we give tokens unique IDs on first creation, we could backtrace to
      * find the most recent common ancestor.
      * - simple cases (same 'token', but different input transform lengths/effects)
      *   will have the same prior token ID
      */
    const primaryInput = tokenizationAnalysis.inputs[0].sample.get(i);
    if(!taillessTrueKeystroke) {
      taillessTrueKeystroke = {...primaryInput};
    } else {
      taillessTrueKeystroke.insert += primaryInput.insert;
      taillessTrueKeystroke.deleteLeft += primaryInput.deleteLeft;
    }
  }

  return taillessTrueKeystroke;
}