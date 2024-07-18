(function() {
'use strict';
var definitions = {
  defaults: {
    version: "16.0.60-alpha",
    applyCasing: function defaultApplyCasing(casing, text) {
    switch (casing) {
        case 'lower':
            return text.toLowerCase();
        case 'upper':
            return text.toUpperCase();
        case 'initial':
            var headCode = text.charCodeAt(0);
            // The length of the first code unit, as measured in code points.
            var headUnitLength = 1;
            // Is the first character a high surrogate, indicating possible use of UTF-16
            // surrogate pairs?  Also, is the string long enough for there to BE a pair?
            if (text.length > 1 && headCode >= 0xD800 && headCode <= 0xDBFF) {
                // It's possible, so now we check for low surrogates.
                var lowSurrogateCode = text.charCodeAt(1);
                if (lowSurrogateCode >= 0xDC00 && lowSurrogateCode <= 0xDFFF) {
                    // We have a surrogate pair; this pair is the 'first' character.
                    headUnitLength++;
                }
            }
            // Capitalizes the first code unit of the string, leaving the rest intact.
            return text.substring(0, headUnitLength).toUpperCase() // head - uppercased
                .concat(text.substring(headUnitLength)); // tail - lowercased
    }
}
  },  model: {
    searchTermToKey: function defaultCasedSearchTermToKey(wordform, applyCasing) {
    // While this is a bit WET, as the basic `defaultSearchTermToKey` exists and performs some of
    // the same functions, repetition is the easiest way to allow the function to be safely compiled
    // with ease by use of `.toString()`.
    return Array.from(wordform
        .normalize('NFKD')
        // Remove any combining diacritics (if input is in NFKD)
        .replace(/[\u0300-\u036F]/g, '')) // end of `Array.from`
        .map(function (c) { return applyCasing('lower', c); })
        .join('');
}
  },
  applyCasing: function(caseToApply, text) {
        return definitions.defaults.applyCasing(caseToApply, text);
      },
  searchTermToKey: function(text) {
      return definitions.model.searchTermToKey(text, definitions.applyCasing);
    }
};
LMLayerWorker.loadModel(new models.TrieModel({"totalWeight":44103,"root":{"type":"internal","weight":13644,"values":["t","e","s","ȼ","n","u","i"],"children":{"t":{"type":"leaf","weight":13644,"entries":[{"key":"tŧe","weight":13644,"content":"TŦE"}]},"e":{"type":"leaf","weight":9134,"entries":[{"key":"e","weight":9134,"content":"E"}]},"s":{"type":"internal","weight":4816,"values":["e","w"],"children":{"e":{"type":"leaf","weight":4816,"entries":[{"key":"sen","weight":4816,"content":"SEN"}]},"w":{"type":"leaf","weight":2621,"entries":[{"key":"sw","weight":2621,"content":"SW̱"}]}}},"ȼ":{"type":"internal","weight":3479,"values":["﷐","s"],"children":{"﷐":{"type":"leaf","weight":3479,"entries":[{"key":"ȼ","weight":3479,"content":"Ȼ"}]},"s":{"type":"leaf","weight":1925,"entries":[{"key":"ȼse","weight":1925,"content":"ȻSE"}]}}},"n":{"type":"leaf","weight":2314,"entries":[{"key":"niƚ","weight":2314,"content":"NIȽ"}]},"u":{"type":"leaf","weight":2298,"entries":[{"key":"u ","weight":2298,"content":"U¸"}]},"i":{"type":"leaf","weight":1988,"entries":[{"key":"i ","weight":1988,"content":"I¸"},{"key":"i","weight":1884,"content":"I"}]}}}}, {
  wordBreaker: wordBreakers['default'],
  searchTermToKey: definitions.searchTermToKey,
}));
})();
