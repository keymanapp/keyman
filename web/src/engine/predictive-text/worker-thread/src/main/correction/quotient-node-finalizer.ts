/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-04-16
 *
 * This file defines tests for a special quotient node type used to finalize
 * corrections for correctable tokens - whether or not they are adjacent to the
 * active caret.
 */

import { SearchNode } from './distance-modeler.js';
import { SearchQuotientNode } from './search-quotient-node.js';
import { SearchQuotientSpur } from './search-quotient-spur.js';
import { TokenResultMapping } from './token-result-mapping.js';

/**
 * A variant of SearchQuotientNode designed to facilitate handling of the last
 * spur of a context token's representation during a multi-token correction
 * search.
 *
 * If `allowPredictions` is false, penalties will be applied for each additional
 * letter that is missing and not yet otherwise modeled.
 *
 * Other important mechanics of this type:
 * - Separate instances should be constructed for each token per tokenization
 *   pattern it may be found within.  Doing so will each tokenization pattern to
 *   receive and process the same results for the token.
 * - It should also be constructed for a token regardless of whether it actually
 *   does occur within multiple tokenizations.  As it is possible that one token
 *   is a "continuation" of a different token without its terminal word
 *   boundary, this will ensure that the "continued" token may still retrieve
 *   results first processed by the "continuation" token.
 */
export class QuotientNodeFinalizer extends SearchQuotientSpur {
  readonly insertLength: number = 0;
  readonly leftDeleteLength: number = 0;

  private allowPrediction: boolean;

  /**
   * Extends an existing SearchQuotientNode (and its correction data) by a keystroke based
   * on a subset of the incoming keystroke's fat-finger distribution.
   *
   * @param parentNode
   * @param inputs
   * @param inputSource Either:
   * 1.  Data about the actual context range represented by `inputs` and
   * its underlying keystroke.
   * 2.  The sample from the incoming distribution that represents data actually
   * applied to the context.  It need not be included within the subset passed to `inputs`.
   */
  constructor(
    parentNode: SearchQuotientNode,
    allowPrediction: boolean
  ) {
    super(parentNode, null, null, parentNode.codepointLength);
    this.allowPrediction = allowPrediction;
  }

  construct(parentNode: SearchQuotientNode): this {
    return new QuotientNodeFinalizer(parentNode, this.allowPrediction) as this;
  }

  buildEdgesFromResults(baseResults: ReadonlyArray<TokenResultMapping>): SearchNode[] {
    const penalizationNodes = baseResults.map((result) => {
      const node: SearchNode = result.buildRemappedNode(this.spaceId);
      const traversal = node.currentTraversal;
      if(traversal.entries.length > 0 || this.allowPrediction) {
        return node;
      } else {
        // Penalize each node based on the number of extra codepoints necessary to
        // reach a completed word.
        let penalty = 0;

        let children: SearchNode[] = [node];
        let childWithEntry: SearchNode;
        do {
          penalty++;
          children = children.flatMap(child => child.buildInsertionEdges(this.spaceId));
          childWithEntry = children.find((c) => c.currentTraversal.entries.length > 0);

          if(children.length == 0) {
            break;
          }
        } while (!childWithEntry && penalty <= 3);

        return childWithEntry;
      }
    });

    return penalizationNodes.filter((n) => !!n);
  };

  public get bestExample(): {text: string, p: number} {
    return this.parents[0].bestExample;
  }

  get edgeKey(): string {
    return `SR${this.parents[0].sourceRangeKey}L${this.codepointLength}EOT`;
  }

  public merge(space: SearchQuotientNode): SearchQuotientNode {
    throw new Error("Merge operations are not supported on QuotientNodeFinalizer instances");
  }

  public split(splitIndex: number): [SearchQuotientNode, SearchQuotientNode][] {
    throw new Error("Split operations are not supported on QuotientNodeFinalizer instances");
  }
}
