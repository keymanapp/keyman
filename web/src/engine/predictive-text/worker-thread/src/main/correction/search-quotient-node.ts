/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-10-09
 *
 * This file defines the predictive-text engine's SearchSpace class, which is used to
 * manage the search-space(s) for text corrections within the engine.
 */

import { LexicalModelTypes } from "@keymanapp/common-types";

import { SearchNode, SearchResult } from "./distance-modeler.js";
import { SearchQuotientSpur } from "./search-quotient-spur.js";
import { SearchQuotientRoot } from "./search-quotient-root.js";

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

let SPACE_ID_SEED = 0;

export function generateSpaceSeed(): number {
  return SPACE_ID_SEED++;
}

type NullPath = {
  type: 'none'
}

type IntermediateSearchPath = {
  type: 'intermediate',
  cost: number
}

type CompleteSearchPath = {
  type: 'complete',
  cost: number,
  finalNode: SearchNode,
  spaceId: number
}

export type PathResult = NullPath | IntermediateSearchPath | CompleteSearchPath;

export interface InputSegment {
  /**
   * The Transform corresponding to the keystroke applied to the true context
   * for this input event.
   *
   * @deprecated Slated for removal within epic/autocorrect.
   */
  trueTransform: Transform;

  /**
   * The transform / transition ID of the corresponding input event.
   */
  transitionId: number | undefined,

  /**
   * Marks the initial index (inclusive) within the insert strings for the
   * corresponding transitions' Transforms that are applied by the corresponding
   * tokenized correction-search input.
   */
  start: number
}

/**
 * Models the properties and portion of an input event applied by a
 * SearchQuotientNode for correction-search purposes.
 */
export interface PathInputProperties {
  /**
   * Denotes the portion of the ongoing input stream represented by the corresponding
   * input distribution(s) of a SearchQuotientNode.
   */
  segment: InputSegment;

  /**
   * Notes the highest probability found in the input event's transform
   * distribution, regardless of whether or not that specific corresponding
   * input is included within the SearchQuotientNode's correction space.
   */
  bestProbFromSet: number;

  /**
   * A unique identifier noting membership in a specific set of input possibilities with
   * sufficiently similar properties that all correspond to the same "input segment".
   *
   * This tends to serve as an identifying factor for tokenized input distributions,
   * indicating the distributions were all sourced from the same original input event.
   *
   * @see TransitionEdge.inputSubsetId
   */
  subsetId: number;
}

/**
 * Represents all or a portion of the dynamically-generated graph used to search
 * for predictive-text corrections.
 */
export interface SearchQuotientNode {
  /**
   * Returns an identifier uniquely identifying this search-batching structure
   * by correction-search results.
   */
  readonly spaceId: number;

  /**
   * Notes the SearchQuotientNode(s) whose correction-search paths are extended by this
   * SearchQuotientNode.
   */
  readonly parents: SearchQuotientNode[];

  /**
   * Retrieves the lowest-cost / lowest-distance edge from the batcher's search
   * area, checks its validity as a correction to the input text, and reports on
   * what sort of result the edge's destination node represents.
   * @returns
   */
  handleNextNode(): PathResult;

  /**
   * Increases the editing range that will be considered for determining
   * correction distances.
   */
  increaseMaxEditDistance(): void;

  /**
   * Reports the cost of the lowest-cost / lowest-distance edge held within the
   * batcher's search area.
   * @returns
   */
  readonly currentCost: number;

  /**
   * Provides a heuristic for the base cost at this path's depth if the best
   * individual input were taken here, regardless of whether or not that's
   * possible.
   *
   * This cost is based on the negative log-likelihood of the probability and
   * includes the cost from the lowest possible parent nodes visited.
   */
  readonly lowestPossibleSingleCost: number;

  /**
   * Returns the set of previously-processed results under this batcher's domain.
   */
  readonly previousResults: SearchResult[];

  /**
   * When true, this indicates that the currently-represented portion of context
   * has fat-finger data available, which itself indicates that the user has
   * corrections enabled.
   */
  readonly correctionsEnabled: boolean;

  /**
   * Reports the total number of input keystrokes represented by this
   * graph/subgraph.
   *
   * (Their fat-finger alternates, when provided, do not influence this count -
   * they're associated with the original keystroke that affected the context.)
   */
  readonly inputCount: number;

  /**
   * Retrieves the sequence of inputs that led to this SearchSpace.
   *
   * THIS WILL BE REMOVED SHORTLY in favor of `constituentPaths` below, which
   * provides an improved view into the data and models multiple paths to the
   * space when they exist.  (Once SearchQuotientNode takes on merging &
   * splitting)
   */
  readonly inputSequence: Distribution<Transform>[];

  /**
   * Reports the length in codepoints of corrected text represented by completed
   * paths from this instance.
   */
  readonly codepointLength: number;

  /**
   * Determines the best example text representable by this batcher's portion of
   * the correction-search graph and its paths.
   */
  readonly bestExample: { text: string, p: number };

  /**
   * Gets components useful for building a string-based representation of the
   * keystroke range corrected by this search space.
   *
   * TODO: will return only the `inputSegment` part of each entry in the future.
   */
  readonly inputSegments: PathInputProperties[];

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  get sourceRangeKey(): string;
}

/**
 * Denotes whether or not the represented search-space quotient path includes
 * paths built from the specified set of keystroke input distributions.  The
 * distribution count should match .inputCount - no omissions or extras are
 * permitted.
 *
 * Designed explicitly for use in unit testing; it's not super-efficient, so
 * avoid live use.
 *
 * @param keystrokeDistributions
 * @internal
 */
function quotientPathHasInputs(node: SearchQuotientNode, keystrokeDistributions: Distribution<Transform>[]): boolean {
  if(!(node instanceof SearchQuotientSpur)) {
    for(const p of node.parents) {
      if(quotientPathHasInputs(p, keystrokeDistributions)) {
        return true;
      }
    }

    return node.parents.length == 0 && keystrokeDistributions.length == 0;
  }

  if(node.inputCount == 0) {
    return keystrokeDistributions.length == 0;
  } else if(keystrokeDistributions.length != node.inputCount) {
    return false;
  }

  const tailInput = [...keystrokeDistributions[keystrokeDistributions.length - 1]];
  keystrokeDistributions = keystrokeDistributions.slice(0, keystrokeDistributions.length - 1);
  const localInput = node.lastInput;

  const parentHasInput = () => !!node.parents.find(p => quotientPathHasInputs(p, keystrokeDistributions));

  // Actual reference match?  Easy mode.
  if(localInput == tailInput) {
    return parentHasInput();
  } else if(localInput.length != tailInput.length) {
    return false;
  } else {
    for(let entry of tailInput) {
      const matchIndex = localInput.findIndex((x) => {
        const s1 = x.sample;
        const s2 = entry.sample;
        // Check for equal reference first before the other checks; it makes a nice shortcut.
        if(x == entry) {
          return true;
        }

        if(x.p == entry.p && s1.deleteLeft == s2.deleteLeft
          && s1.id == s2.id && ((s1.deleteRight ?? 0) == (s2.deleteRight ?? 0)) && s1.insert == s2.insert
        ) {
          return true;
        }

        return false;
      });

      if(matchIndex == -1) {
        return false;
      } else {
        tailInput.splice(matchIndex, 1);
      }
    }

    return parentHasInput();
  }
}

/**
 * Enumerates the different potential SearchQuotientSpur sequences that lead
 * to a SearchQuotientNode.
 *
 * Intended only for use during unit testing.  Does not include the root node.
 */
function constituentPaths(node: SearchQuotientNode): SearchQuotientSpur[][] {
  if(node instanceof SearchQuotientRoot) {
    return [];
  } else if(node instanceof SearchQuotientSpur) {
    const parentPaths = constituentPaths(node.parents[0]);
    if(parentPaths.length > 0) {
      return parentPaths.map(p => {
        p.push(node);
        return p;
      });
    } else {
      return [[node]];
    }
  } else {
    throw new Error("constituentPaths is unable to handle a new, unexpected SearchQuotientNode type");
  }
}

export const unitTestEndpoints = {
  quotientPathHasInputs,
  constituentPaths
}