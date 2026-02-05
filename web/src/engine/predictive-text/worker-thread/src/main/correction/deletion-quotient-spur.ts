/**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-02-03
 *
 * This file adds a SearchQuotientSpur variant modeling deletion of the corresponding
 * keystroke.
 */

import { LexicalModelTypes } from "@keymanapp/common-types";

import { SearchNode } from "./distance-modeler.js";
import { PathInputProperties, SearchQuotientNode } from "./search-quotient-node.js";
import { SearchQuotientSpur } from "./search-quotient-spur.js";

import Distribution = LexicalModelTypes.Distribution;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

export class DeletionQuotientSpur extends SearchQuotientSpur {
  public readonly insertLength: number = 0;
  public readonly leftDeleteLength: number = 0;

  constructor(
    parentNode: SearchQuotientNode,
    inputs: Distribution<Readonly<Transform>>,
    inputSource: PathInputProperties | ProbabilityMass<Transform>
  ) {
    super(parentNode, inputs, inputSource, parentNode.codepointLength);
  }

  construct(parentNode: SearchQuotientNode, inputs: ProbabilityMass<Readonly<Transform>>[], inputSource: PathInputProperties): this {
    return new DeletionQuotientSpur(parentNode, inputs, inputSource) as this;
  }

  protected buildEdgesForNodes(baseNodes: ReadonlyArray<SearchNode>): SearchNode[] {
    return baseNodes.flatMap((n) => n.buildDeletionEdges(this.inputs, this.spaceId));
  }

  get edgeKey(): string {
    const baseKey = super.edgeKey;
    return `${baseKey}DEL`;
  }
}