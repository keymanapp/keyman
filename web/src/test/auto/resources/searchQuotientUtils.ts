import { LexicalModelTypes } from "@keymanapp/common-types";

import { SearchQuotientNode, SearchQuotientRoot, SearchQuotientSpur } from "@keymanapp/lm-worker/test-index";

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

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
export function quotientPathHasInputs(node: SearchQuotientNode, keystrokeDistributions: Distribution<Transform>[]): boolean {
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
export function constituentPaths(node: SearchQuotientNode): SearchQuotientSpur[][] {
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