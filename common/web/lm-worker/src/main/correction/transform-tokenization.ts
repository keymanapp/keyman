import { applyTransform, type Tokenization } from "@keymanapp/models-templates";

export function tokenizeTransform(
  tokenize: (context: Context) => Tokenization,
  context: Context,
  transform: Transform
): Transform[] {
  // Context does not slide within this function.
  const postContext = applyTransform(transform, context);
  const postTokenization = tokenize(postContext).left;

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

// If and when we look to do phrase-based suggestions and/or auto-correction on accidental
// spaces, this function should prove useful.
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