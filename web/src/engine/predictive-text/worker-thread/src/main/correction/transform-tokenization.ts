import { LexicalModelTypes } from '@keymanapp/common-types';
import { applyTransform, type Tokenization } from "@keymanapp/models-templates";

import { determineModelTokenizer } from '../model-helpers.js';

import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;
import { computeAlignment } from './alignment-helpers.js';
import { KMWString } from '@keymanapp/web-utils';

/**
 * Determines a tokenization-aware sequence of (`Transform`) edits, one per
 * token that would result after applying the incoming keystroke's `Transform`
 * to its base `Context`.  This sequence reproduces the same net effect as the
 * original incoming `transform` when applied in sequence.  The component
 * transforms are then indexed relative to the position of the corresponding
 * token in the base `Context`, with 0 matching the original token left of
 * the text insertion point, negative indices affecting previous tokens, and
 * positive indicies affecting new tokens.
 *
 * For example, using English and standard whitespace-based tokenization:
 * - context: `the quick blue`
 * - transform: `{ insert: 'rown fox', deleteLeft: 3 }`
 * - Resulting context: `the quick brown fox`
 * - Output: `Map {
 *     0 => { insert: 'rown', deleteLeft: 3 },
 *     1 => { insert: ' ',    deleteLeft: 0 },
 *     2 => { insert: 'fox',  deleteLeft: 0 }
 *   }`
 * @param tokenize The tokenization function to utilize, as determined by the
 * active lexical model or its settings.
 * @param context  The original, unmodified context
 * @param transform  A specification of incoming edits to `context`.
 * @returns
 */
export function tokenizeTransform(
  tokenize: (context: Context) => Tokenization,
  context: Context,
  transform: Transform
): Map<number, Transform> {
  if(transform.insert == '' && transform.deleteLeft == 0) {
    const map = new Map<number, Transform>();
    map.set(0, { insert: '', deleteLeft: 0 });
    return map;
  }

  // Context does not slide within this function.
  const postContext = applyTransform(transform, context);
  const preTokenization  = tokenize(context).left;
  const postTokenization = tokenize(postContext).left;
  if(preTokenization.length == 0) {
    preTokenization.push({text: ''});
  }
  if(postTokenization.length == 0) {
    postTokenization.push({text: ''});
  }

  const alignment = computeAlignment(
    preTokenization.map(t => t.text),
    postTokenization.map(t => t.text),
    false,
    true
  );

  if(!alignment.canAlign || alignment.leadTokenShift) {
    throw new Error(`Could not align context ${JSON.stringify(context)} before and after transform ${JSON.stringify(transform)}`);
  }

  const baseIndex = Math.min(1, 1 - alignment.tailEditLength) + Math.min(0, alignment.tailTokenShift);

  let deleteLeft = transform.deleteLeft;
  const tokenizedTransforms: Transform[] = [];
  // Create deletion transforms for deleted ...
  for(let index = -alignment.tailTokenShift; index > 0; index--) {
    const deletedToken = preTokenization.pop();
    const srcLen = KMWString.length(deletedToken.text);
    deleteLeft -= KMWString.length(deletedToken.text);

    tokenizedTransforms.push({ insert: '', deleteLeft: srcLen });
  }

  let insert = transform.insert;
  // Avoid emitting an empty transform if we land right at the end of a previous
  // token (say, after a backspace)
  if(insert.length > 0 || deleteLeft > 0) {
    for(let index = postTokenization.length - 1; index >= 0; index--) {
      const currentToken = postTokenization[index];
      const srcLen = KMWString.length(preTokenization[index]?.text ?? '');
      const curLen = KMWString.length(currentToken.text);

      if(srcLen >= deleteLeft && curLen >= KMWString.length(insert)) {
        tokenizedTransforms.push({
          insert: insert,
          deleteLeft: deleteLeft
        });
        break;
      }

      insert = insert.substring(0, insert.length - currentToken.text.length);
      deleteLeft = Math.max(0, deleteLeft - srcLen);
      tokenizedTransforms.push({
        insert: currentToken.text,
        deleteLeft: srcLen
      });
    }
  }

  const returnedMap = new Map<number, Transform>();
  let pushedCount: number = 0;
  while(tokenizedTransforms.length > 0) {
    returnedMap.set(baseIndex + pushedCount, tokenizedTransforms.pop());
    pushedCount++;
  }

  return returnedMap;
}

/**
 * Given an incoming distribution of Transforms, this method applies
 * `tokenizeTransform` for each, mapping each transform to its tokenized form in
 * the returned distribution.
 *
 * It is believed that this may prove useful in the future for phrase-based
 * suggestions and/or auto-correction of accidentally-typed whitespace.  For
 * now, it already sees limited use in preventing replacement of word-adjacent
 * punctuation marks.
 * @param tokenize
 * @param context
 * @param transformDistribution
 * @returns
 */
export function tokenizeTransformDistribution(
  tokenize: (context: Context) => Tokenization,
  context: Context,
  transformDistribution: Distribution<Transform>
): Distribution<Map<number, Transform>> {
  return transformDistribution.map((transform) => {
    return {
      sample: tokenizeTransform(tokenize, context, transform.sample),
      p: transform.p
    };
  });
}

/**
 * Given an incoming distribution of Transforms, this method applies
 * `tokenizeTransform` for each, mapping each transform to its tokenized form in
 * the returned distribution.
 *
 * It then filters out all incoming Transforms that do not result in the same final
 * number of tokens as the "primary input" when applied, as the context-tracker
 * and predictive-text engine cannot handle word-breaking divergence well at
 * this time.
 * @param context
 * @param model
 * @param transformDistribution
 * @returns
 */
export function tokenizeAndFilterDistribution(
  context: Context,
  model: LexicalModel,
  transformDistribution?: Distribution<Transform>
): Distribution<Map<number, Transform>> {
  let tokenize = determineModelTokenizer(model);
  const inputTransform = transformDistribution?.[0];

  if(!inputTransform) {
    return null;
  }

  // These two methods apply transforms internally; do not mutate context here.
  // This particularly matters for the 'distribution' variant.
  const tokenizedInputTransform = tokenizeTransform(tokenize, context, inputTransform.sample);
  const lastTokenizedInputIndex = [...tokenizedInputTransform.keys()].reverse()[0];
  const tokenizedDistribution = tokenizeTransformDistribution(tokenize, context, transformDistribution);

  // While we lack phrase-based / phrase-oriented prediction support, we'll just extract the
  // set that matches the token length that results from our input.
  return tokenizedDistribution.filter((entry) =>
    entry.sample.has(lastTokenizedInputIndex) && !entry.sample.has(lastTokenizedInputIndex + 1)
  );
}