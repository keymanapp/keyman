import { LexicalModelTypes } from '@keymanapp/common-types';
import { SENTINEL_CODE_UNIT } from '@keymanapp/models-templates';
import { KMWString } from '@keymanapp/web-utils';

import { ContextTokenization, TokenizationEdgeAlignment, TokenizationTransitionEdits } from './context-tokenization.js';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

/**
 * Defines a subset of pending tokenization transitions based on potential inputs.
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
  readonly pendingSet: Map<ContextTokenization, {
    /**
     * The edge window corresponding to the common tokenization for the subset's inputs
     */
    alignment: TokenizationEdgeAlignment,
    /**
     * A set of incoming keystrokes with compatible effects when applied.
     *
     * If passed to `subsetByInterval`, the transforms should result in a single subset.
     */
    tokenizedInputs: Distribution<Map<number, Transform>>
  }>;
}

export function precomputationSubsetKeyer(tokenizationEdits: TokenizationTransitionEdits): string {
  const { alignment, tokenizedTransform } = tokenizationEdits;
  const { edgeWindow, merges, splits, unmappedEdits } = alignment;
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
  const boundaryComponent = `B${editBoundary.tokenIndex}=${editBoundary.sourceRangeKey}`;

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
      components.push(`BI@${relativeIndex}-${boundaryTextLen + insertLen}`);
      boundaryTextLen = 0;
    } else {
      components.push(`I@${relativeIndex}-${insertLen}`);
    }
  }

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

  return components.join(SENTINEL_CODE_UNIT);
}

export class TokenizationSubsetBuilder {
  private _subsets: Map<string, TokenizationSubset> = new Map();

  addPrecomputation(tokenization: ContextTokenization, precomputation: TokenizationTransitionEdits, p: number) {
    const key = precomputationSubsetKeyer(precomputation);

    // Should file the object and its transform data appropriately.
    const entry: TokenizationSubset = this._subsets.get(key) ?? {
      pendingSet: new Map(),
      key: key
    }
    const forTokenization = entry.pendingSet.get(tokenization) ?? {
      alignment: precomputation.alignment,
      tokenizedInputs: []
    };
    forTokenization.tokenizedInputs.push({sample: precomputation.tokenizedTransform, p});
    entry.pendingSet.set(tokenization, forTokenization);
    this._subsets.set(key, entry);
  }

  get subsets(): ReadonlyMap<string, TokenizationSubset> {
    return this._subsets;
  }
}
