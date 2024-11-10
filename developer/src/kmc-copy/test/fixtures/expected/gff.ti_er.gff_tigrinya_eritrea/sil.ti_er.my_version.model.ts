/*
  GFF Tigrinya-Ertirean language lexical model.
*/

const source: LexicalModelSource = {
  format: 'trie-1.0',
  wordBreaker: 'default',
  sources: ['TigrinyaErWordList.tsv'],
};
export default source;
