/**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-02-03
 *
 * This file adds a SearchQuotientSpur variant modeling insertion of
 * lexical-entry prefix characters - an operation with no corresponding
 * keystroke.
 */

import { SearchNode } from "./distance-modeler.js";
import { SearchQuotientNode } from "./search-quotient-node.js";
import { SearchQuotientSpur } from "./search-quotient-spur.js";

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

  protected buildEdgesForNodes(baseNodes: ReadonlyArray<SearchNode>): SearchNode[] {
    // Note that .buildInsertionEdges will not extend any nodes reached by empty-input
    // or by deletions.
    return baseNodes.flatMap((n) => n.buildInsertionEdges());
  }

  get edgeKey(): string {
    return `SR[${this.parentNode.sourceRangeKey}]L${this.codepointLength}INS`;
    // How will this differentiate from other cases?
    // ... sourceRangeKey + codepointLength + insert count?
    return '';
  }
}