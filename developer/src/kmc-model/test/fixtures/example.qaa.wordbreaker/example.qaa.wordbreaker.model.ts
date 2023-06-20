const source: LexicalModelSource = {
  format: 'trie-1.0',
  sources: ['wordlist.tsv'],
  /* Keyman 14.0+ word breaker specification: */
  wordBreaker: {
    use: 'default'
  }
};
export default source;
