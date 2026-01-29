import { LexicalModelTypes } from '@keymanapp/common-types';
import { SENTINEL_CODE_UNIT } from '@keymanapp/models-templates';
import { KMWString } from '@keymanapp/web-utils';

import { ContextTokenization, TransitionEdgeAlignment, TokenizationTransitionEdits } from './context-tokenization.js';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

let SUBSET_ID_SEED = 0;

export function generateSubsetId() {
  return SUBSET_ID_SEED++;
}

/**
 * Tracks metadata about the input "edge" transitioning from one source
 * ContextTokenization to a potentially-common destination ContextTokenization.
 *
 * Once evaluated, each entry within its `.inputs` field should have a
 * one-to-one relationship with instances of the `SearchPath` class.
 */
export interface TransitionEdge {
  /**
   * The edge window corresponding to the common ContextTokenization context
   * to which this path's inputs will be applied.
   */
  alignment: TransitionEdgeAlignment,

  /**
   * A set of incoming keystrokes with compatible effects when applied.
   *
   * If passed to the `subsetByInterval`, the transforms should result in a single subset.
   */
  inputs: Distribution<Map<number, Transform>>

  /**
   * A unique identifier associated with this TransitionEdge and its
   * transforms within `SearchSpace`s.  This ID assists with detecting when
   * split transforms are re-merged during SearchSpace merges.  Only
   * input-sources with matching subset ID come from the same subset, and thus
   * only they should be candidates for re-merging a previous split.
   *
   * The subset ID does not necessarily match the transition ID; in fact, there
   * may be a one-to-many relationship between transition ID and
   * `inputSubsetId`.  Note that the original transition ID may be found within
   * each `Transform` value entry found within the `.inputs` map if desired.
   */
  inputSubsetId: number;
}

/**
 * Defines a subset of pending tokenization transitions based on potential
 * inputs.
 *
 * If more than one `transitionEdges` entry exists, this should directly
 * correspond to a unique instance of `SearchCluster` (per affected
 * `ContextToken`) once fully processed, each comprised of the corresponding
 * `SearchPath` entries constructed from each `transitionEdges` entry.
 *
 * If only one `transitionEdges` entry exists, it should correspond to
 * `SearchPath` instances instead; there is no need for `SearchCluster` overhead
 * in such cases.
 */
export interface TokenizationSubset {
  /**
   * A key that matches for any tokenization transitions that yield compatible
   * result search paths.
   */
  readonly key: string;
  /**
   * A set of pre-existing tokenizations and transforms that may be input to
   * them, yielding compatible search paths and tokenization effects after their
   * application.
   */
  readonly transitionEdges: Map<ContextTokenization, TransitionEdge>;
}

export function editKeyer(precomputation: TokenizationTransitionEdits): string[] {
  const { merges, splits, unmappedEdits } = precomputation.alignment;
  const components: string[] = [];

  if(merges.length > 0) {
    components.push('M:' + merges.map((matchMap) => {
      // Text may be more unique, but is likely unnecessary; index yields shorter,
      // easier to process keys.
      const inputPortion = matchMap.inputs.map(i => '' + i.index).join('+');
      return `M:${inputPortion}=>${matchMap.match.index}`;
    }).join(','));
  }

  if(splits.length > 0) {
    components.push('S:' + splits.map((matchMap) => {
      // Text may be more unique, but is likely unnecessary; index yields shorter,
      // easier to process keys.
      const matchPortion = matchMap.matches.map(m => '' + m.index).join('+');
      return `${matchMap.input.index}=>${matchPortion}`;
    }).join(','));
  }

  if(unmappedEdits.length > 0) {
    // We really shouldn't have these, let alone often.
    components.push('UE:' + unmappedEdits.map((edit) => {
      return `${edit.op}(${edit.input ?? ''}-${edit.match ?? ''}`;
    }).join(','));
  }

  return components;
}

export function legacySubsetKeyer(tokenizationEdits: TokenizationTransitionEdits): string {
  const { alignment, tokenizedTransform } = tokenizationEdits;
  const { edgeWindow, merges, splits } = alignment;
  const components: string[] = [];

  // First entry: based on the edge window.  The real key:  what's the edit
  // boundary?  We need to apply to the same token and portion thereof.
  const editBoundary = edgeWindow.editBoundary;

  // For the legacy keyer, all we care about is that we land within the same
  // token. We simply note the boundary token's index within the edge window.
  const boundaryEdgeIndex = editBoundary.tokenIndex - edgeWindow.sliceIndex;

  // Identify the new boundary token's length - as it appears after any related
  // merges or splits.
  let boundaryTextLen = KMWString.length(editBoundary.text);
  const boundaryMerge = merges.find((m) => m.inputs.find(i => i.index == boundaryEdgeIndex));
  const boundarySplit = splits.find((s) => s.input.index == boundaryEdgeIndex);
  if(boundaryMerge) {
    boundaryTextLen = KMWString.length(boundaryMerge.match.text);
  } else if(boundarySplit) {
    boundaryTextLen = KMWString.length(boundarySplit.matches[boundarySplit.matches.length - 1].text);
  }

  // Now, based on the transform tokenization. We want to force uniqueness for
  // all variations of result length on each tokenized transform resulting from
  // the precomputation's represented keystroke.
  for(const {0: relativeIndex} of tokenizedTransform.entries()) {
    if(relativeIndex > 0) {
      // The true boundary lie before the insert if the value is non-zero;
      // don't differentiate here!
      boundaryTextLen = 0;
    }

    if(boundaryTextLen) {
      // transform.deleteLeft was already handled during boundary computation -
      // do not include it here!
      //
      // IMPORTANT:  update unit tests manually if the BI marker here changes
      // or the use of SENTINEL_CODE_UNIT as a key component separator changes.
      components.push(`BI@${relativeIndex}`);
      boundaryTextLen = 0;
    } else {
      components.push(`I@${relativeIndex}`);
    }
  }

  return components.concat(editKeyer(tokenizationEdits)).join(SENTINEL_CODE_UNIT);
}

export function precomputationSubsetKeyer(tokenizationEdits: TokenizationTransitionEdits): string {
  const { alignment, tokenizedTransform } = tokenizationEdits;
  const { edgeWindow, merges, splits } = alignment;
  const components: string[] = [];

  // First entry: based on the edge window.  The real key:  what's the edit
  // boundary?  We need to apply to the same token and portion thereof.
  const editBoundary = edgeWindow.editBoundary;

  // It's not about the boundary text - we just need to ensure it's the 'same'
  // token - comprised of the same keystrokes.  `sourceRangeKey` reflects the
  // actual input for the source keystrokes.  We might have deleted part of it
  // in this tokenization, but that doesn't matter here - we want to imply the
  // represented keystroke range.
  const boundaryEdgeIndex = editBoundary.tokenIndex - edgeWindow.sliceIndex;
  const boundaryComponent = `B${editBoundary.tokenIndex}=${editBoundary.sourceRangeKey}`; // source range is part of it

  components.push(boundaryComponent);

  // Identify the new boundary token's length - as it appears after any related
  // merges or splits.
  let boundaryTextLen = KMWString.length(editBoundary.text);
  const boundaryMerge = merges.find((m) => m.inputs.find(i => i.index == boundaryEdgeIndex));
  const boundarySplit = splits.find((s) => s.input.index == boundaryEdgeIndex);
  if(boundaryMerge) {
    boundaryTextLen = KMWString.length(boundaryMerge.match.text);
  } else if(boundarySplit) {
    boundaryTextLen = KMWString.length(boundarySplit.matches[boundarySplit.matches.length - 1].text);
  }

  // Now, based on the transform tokenization. We want to force uniqueness for
  // all variations of result length on each tokenized transform resulting from
  // the precomputation's represented keystroke.
  for(const {0: relativeIndex, 1: transform} of tokenizedTransform.entries()) {
    const insertLen = KMWString.length(transform.insert);
    if(relativeIndex > 0) {
      // The true boundary lies before the insert if the value is non-zero;
      // don't differentiate here!
      boundaryTextLen = 0;
    }

    if(boundaryTextLen) {
      // transform.deleteLeft was already handled during boundary computation -
      // do not include it here!
      //
      // IMPORTANT:  update unit tests manually if the BI marker here changes
      // or the use of SENTINEL_CODE_UNIT as a key component separator changes.
      components.push(`BI@${relativeIndex}-${boundaryTextLen + insertLen}`);
      boundaryTextLen = 0;
    } else {
      components.push(`I@${relativeIndex}-${insertLen}`);
    }
  }

  return components.concat(editKeyer(tokenizationEdits)).join(SENTINEL_CODE_UNIT);
}

export class TokenizationSubsetBuilder {
  private _subsets: Map<string, TokenizationSubset> = new Map();
  readonly keyer: typeof precomputationSubsetKeyer;

  constructor(keyer?: typeof precomputationSubsetKeyer) {
    this.keyer = keyer ?? precomputationSubsetKeyer;
  }

  addPrecomputation(tokenization: ContextTokenization, precomputation: TokenizationTransitionEdits, p: number) {
    const key = this.keyer(precomputation);

    // Should file the object and its transform data appropriately.
    //
    // Maps any number of Tokenizations and their incoming alignment data to a common key
    // for final tokenization forms.
    const entry: TokenizationSubset = this._subsets.get(key) ?? {
      transitionEdges: new Map(),
      key: key
    }

    // Finds any previously-accumulated data corresponding to both the incoming and
    // target final tokenization form, creating an empty entry if none yet exists.
    const forTokenization: TransitionEdge = entry.transitionEdges.get(tokenization) ?? {
      alignment: precomputation.alignment,
      inputs: [],
      inputSubsetId: generateSubsetId()
    };

    // Adds the incoming tokenized transform data for the pairing...
    forTokenization.inputs.push({sample: precomputation.tokenizedTransform, p});
    // and ensures that the pairing's data-accumulator is in the map.
    entry.transitionEdges.set(tokenization, forTokenization);

    // Also ensures that the target tokenization's data (accumulating the pairings)
    // is made available within the top-level map.
    this._subsets.set(key, entry);
  }

  get subsets(): ReadonlyMap<string, TokenizationSubset> {
    return this._subsets;
  }
}
