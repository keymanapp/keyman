/**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-02-03
 *
 * This file adds a SearchQuotientSpur variant modeling insertion of
 * lexical-entry prefix characters - an operation with no corresponding
 * keystroke.
 */

import { SENTINEL_CODE_UNIT } from "@keymanapp/models-templates";
import { SearchNode } from "./distance-modeler.js";
import { SearchQuotientNode } from "./search-quotient-node.js";
import { SearchQuotientSpur } from "./search-quotient-spur.js";
import { TokenResultMapping } from "./token-result-mapping.js";

export class InsertionQuotientSpur extends SearchQuotientSpur {
  public readonly insertLength = 1;
  public readonly leftDeleteLength = 0;

  constructor(
    parentNode: SearchQuotientNode
  ) {
    super(parentNode, null, null, parentNode.codepointLength + 1);
  }

  construct(parentNode: SearchQuotientNode): this {
    return new InsertionQuotientSpur(parentNode) as this;
  }

  protected buildEdgesFromResults(baseNodes: ReadonlyArray<TokenResultMapping>): SearchNode[] {
    // Note that .buildInsertionEdges will not extend any nodes reached by empty-input
    // or by deletions.
    return baseNodes
      // If there are already at least 2 edits for a node, do not add new edits.
      // Also, do not permit insert edits to follow delete edits.
      .filter((n) => n.lastEdgeType != 'deletion' && n.editCount < 2)
      .flatMap((n) => n.buildInsertionEdges(this.spaceId));
  }

  get edgeKey(): string {
    return `SR[${this.parentNode.sourceRangeKey}]L${this.codepointLength}INS`;
  }

  get bestExample() {
    const base = this.parentNode.bestExample;
    // We use the SENTINEL char as an insertion place-holder, as there's no
    // actual keystroke to source better characters from.
    base.text += SENTINEL_CODE_UNIT;

    return base;
  }
}