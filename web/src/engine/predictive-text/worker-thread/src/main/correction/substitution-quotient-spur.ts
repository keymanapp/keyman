/**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-02-03
 *
 * This file adds a SearchQuotientSpur variant modeling match & substitute edit
 * operations in regard to the corresponding keystroke.
 */

import { LexicalModelTypes } from "@keymanapp/common-types";
import { KMWString } from "@keymanapp/web-utils";

import { SearchNode } from "./distance-modeler.js";
import { PathInputProperties, SearchQuotientNode } from "./search-quotient-node.js";
import { SearchQuotientSpur } from "./search-quotient-spur.js";

import Distribution = LexicalModelTypes.Distribution;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;

export class SubstitutionQuotientSpur extends SearchQuotientSpur {
  public insertLength: number;
  public leftDeleteLength: number;

  constructor(
    parentNode: SearchQuotientNode,
    inputs: Distribution<Readonly<Transform>>,
    inputSource: PathInputProperties | ProbabilityMass<Transform>
  ) {
    // Compute this SearchPath's codepoint length & edge length.
    const inputSample = inputs?.[0].sample ?? { insert: '', deleteLeft: 0 };
    const insertLength = KMWString.length(inputSample.insert);
    super(parentNode, inputs, inputSource, parentNode.codepointLength + insertLength - inputSample.deleteLeft);

    // Compute this SearchPath's codepoint length & edge length.
    this.insertLength = insertLength;
    this.leftDeleteLength = inputSample.deleteLeft;
  }

  construct(parentNode: SearchQuotientNode, inputs: ProbabilityMass<Readonly<Transform>>[], inputSource: PathInputProperties): this {
    return new SubstitutionQuotientSpur(parentNode, inputs, inputSource) as this;
  }

  protected buildEdgesForNodes(baseNodes: ReadonlyArray<SearchNode>): SearchNode[] {
    return baseNodes.flatMap((n) => n.buildSubstitutionEdges(this.inputs, this.spaceId));
  }
}