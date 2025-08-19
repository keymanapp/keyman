import { LexicalModelTypes } from '@keymanapp/common-types';
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;
import { applyTransform, type Tokenization } from "@keymanapp/models-templates";
import { determineModelTokenizer } from '#./model-helpers.js';

/**
 * Determines a tokenization-aware sequence of (`Transform`) edits, one per
 * token that would result after applying the incoming keystroke's `Transform`
 * to its base `Context`.  This sequence reproduces the same net effect as the
 * original incoming `transform` when applied in sequence.
 *
 * For example, using English and standard whitespace-based tokenization:
 * - context: `the quick blue`
 * - transform: `{ insert: 'rown fox', deleteLeft: 3 }`
 * - Resulting context: `the quick brown fox`
 * - Output: `[
 *     { insert: 'rown', deleteLeft: 3 },
 *     { insert: ' ',    deleteLeft: 0 },
 *     { insert: 'fox',  deleteLeft: 0 }
 *   ]`
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
): Transform[] {
  // Context does not slide within this function.
  const postContext = applyTransform(transform, context);
  const postTokenization = tokenize(postContext).left;
  if(postTokenization.length == 0) {
    postTokenization.push({text: ''});
  }

  let insert = transform.insert;

  const tokenizedTransforms: Transform[] = [];
  for(let index = postTokenization.length - 1; index >= 0; index--) {
    const currentToken = postTokenization[index];
    const textLen = currentToken.text.length;

    if(textLen < insert.length) {
      tokenizedTransforms.unshift({
        insert: currentToken.text,
        deleteLeft: 0
      });

      insert = insert.substring(0, insert.length - textLen);
    } else {
      tokenizedTransforms.unshift({
        insert: insert,
        deleteLeft: transform.deleteLeft
      });
      break;
    }
  }

  return tokenizedTransforms;
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
): Distribution<Transform[]> {
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
 * It then filters out all incoming Transforms that do not result in the same
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
) {
  let tokenize = determineModelTokenizer(model);
  const inputTransform = transformDistribution?.[0];

  let transformTokenLength = 0;
  let tokenizedDistribution: Distribution<Transform[]> = null;
  if(inputTransform) {
    // These two methods apply transforms internally; do not mutate context here.
    // This particularly matters for the 'distribution' variant.

    // What if a pre-whitespace token has a final substitution as PART of an edit?
    // Say, ['apple', ' ', ''] => ['apply', ' ', 'n']
    // For now... we can't really handle that case well - modeling the 'e' => 'y' part.
    // Will likely require improvements to tokenizeTransform(), which doesn't yet handle
    // deleteLeft tokenization for transforms spanning tokens & whitespace.
    //
    // See: #14361.
    // There's a good shot attemptTokenizedAlignment would be useful for it.
    transformTokenLength = tokenizeTransform(tokenize, context, inputTransform.sample).length;
    tokenizedDistribution = tokenizeTransformDistribution(tokenize, context, transformDistribution);

    // Now we update the context used for context-state management based upon our input.
    context = applyTransform(inputTransform.sample, context);

    // While we lack phrase-based / phrase-oriented prediction support, we'll just extract the
    // set that matches the token length that results from our input.
    tokenizedDistribution = tokenizedDistribution.filter((entry) => entry.sample.length == transformTokenLength);
  }

  return tokenizedDistribution;
}