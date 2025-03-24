const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  // CUSTOMIZE THIS:
  punctuation: {
    quotesForKeepSuggestion: {
      open: "“", close: "”"
    },
    insertAfterWord: " ",
  },
  // other customizations go here:
};

export default source;
