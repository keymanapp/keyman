/**
 * The custom wordbreaker used by the sil.km.gcc model as of
 * https://github.com/keymanapp/lexical-models/pull/265.
 * @type {WordBreakingFunction}
 * */
export function customWordBreakerProper(str) {
  const whitespaceRegex = /\s|\u200b|\n|\r/;
  const tokens = str.split(whitespaceRegex);

  for(let i=0; i < tokens.length; i++) {
    const token = tokens[i];
    if(token.length == 0) {
      tokens.splice(i, 1);
      i--;
      continue;
    } else if(token.length == 1 && whitespaceRegex.test(token)) {
      tokens.splice(i, 1);
      i--;
      continue;
    }

    // Certain punctuation marks should be considered a separate token from the word they're next to.
    const punctuationMarks = ['«', '»', '$', '#' /* add extras here */];
    const punctSplitIndices = [];

    // Find if and where each mark exists within the token
    for(let i = 0; i < punctuationMarks.length; i++) {
      const split = token.indexOf(punctuationMarks[i]);
      if(split >= 0) {
        punctSplitIndices.push(split);
      }
    }

    // Sort and pick the earliest mark's location.  If none exists, use -1.
    punctSplitIndices.sort();
    const splitPoint = punctSplitIndices[0] === undefined ? -1 : punctSplitIndices[0];

    if(splitPoint > -1) {
      const left = token.substring(0, splitPoint);  // (0, -1) => ''
      const punct = token.substring(splitPoint, splitPoint+1);
      const right = token.substring(splitPoint+1);  // Starting past the end of the string => ''

      if(left) {
        tokens.splice(i++, 0, left);
      }
      tokens.splice(i++, 1, punct);
      if(right) {
        tokens.splice(i, 0, right);
      }
      // Ensure that the next iteration puts `i` immediately after the punctuation token... even if
      // there was a `right` portion, as it may have extra marks that also need to be spun off.
      i--;
    }
  }

  let latestIndex = 0;
  return tokens.map(function(token) {
    const start = str.indexOf(token, latestIndex);
    latestIndex = start + token.length;
    return {
      left: start,
      start: start,
      right: start + token.length,
      end: start + token.length,
      length: token.length,
      text: token
    }
  });
}

/**
 * The version of the custom wordbreaker used by the sil.km.gcc model
 * before https://github.com/keymanapp/lexical-models/pull/265, which
 * triggered #12200.
 * @type {WordBreakingFunction}
 * */
export function customWordBreakerFormer (str) {
  const tokens = str.split(/\s|\u200b/);

  for(let i=0; i < tokens.length; i++) {
    const token = tokens[i];
    if(token.length == 1) {
      continue;
    }

    // Certain punctuation marks should be considered a separate token from the word they're next to.
    const punctuationMarks = ['«', '»', '$', '#' /* add extras here */];
    const punctSplitIndices = [];

    // Find if and where each mark exists within the token
    for(let i = 0; i < punctuationMarks.length; i++) {
      const split = token.indexOf(punctuationMarks[i]);
      if(split >= 0) {
        punctSplitIndices.push(split);
      }
    }

    // Sort and pick the earliest mark's location.  If none exists, use -1.
    punctSplitIndices.sort();
    const splitPoint = punctSplitIndices[0] === undefined ? -1 : punctSplitIndices[0];

    if(splitPoint > -1) {
      const left = token.substring(0, splitPoint);  // (0, -1) => ''
      const punct = token.substring(splitPoint, splitPoint+1);
      const right = token.substring(splitPoint+1);  // Starting past the end of the string => ''

      if(left) {
        tokens.splice(i++, 0, left);
      }
      tokens.splice(i++, 1, punct);
      if(right) {
        tokens.splice(i, 0, right);
      }
      // Ensure that the next iteration puts `i` immediately after the punctuation token... even if
      // there was a `right` portion, as it may have extra marks that also need to be spun off.
      i--;
    }
  }
  return tokens.map(function(token) {
    return {
      left: str.indexOf(token),
      start: str.indexOf(token),
      right: str.indexOf(token) + token.length,
      end: str.indexOf(token) + token.length,
      text: token
    }
  });
}