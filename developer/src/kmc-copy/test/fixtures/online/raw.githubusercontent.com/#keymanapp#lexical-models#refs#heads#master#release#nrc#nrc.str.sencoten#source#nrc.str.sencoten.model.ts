const source: LexicalModelSource = {
  format: 'trie-1.0',
  wordBreaker: 'default',
  sources: ['saanich.tsv'],
  searchTermToKey: function (wordform: string): string {
    const CEDILLA = '¸';
    const COMBINING_ACUTE_ACCENT = '\u0301'; // ÍÁŚḰĆ
    const COMBINING_LONG_SOLIDUS_OVERLAY = '\u0338'; // ȾȺȻ
    const COMBINING_MACRON_BELOW = '\u0331'; // W̱ṮḴX̱Ṉ
    const KIP_SIGN = "\u20AD" // ₭
    const LATIN_CAPITAL_LETTER_A_WITH_STROKE = '\u023A' // Ⱥ
    const LATIN_CAPITAL_LETTER_C_WITH_STROKE = '\u023B'; // Ȼ
    const LATIN_CAPITAL_LETTER_L_WITH_BAR = "\u023D"; // Ƚ
    const LATIN_CAPITAL_LETTER_T_WITH_DIAGONAL_STROKE = '\u023E'; // Ⱦ
    const LATIN_CAPITAL_LETTER_T_WITH_STROKE = '\u0166'; // Ŧ

    return wordform.normalize('NFD')
      .toUpperCase()
      .replace(CEDILLA, '')
      .replace(COMBINING_ACUTE_ACCENT, '')
      .replace(COMBINING_LONG_SOLIDUS_OVERLAY, '')
      .replace(COMBINING_MACRON_BELOW, '')
      .replace(KIP_SIGN, 'K')
      .replace(LATIN_CAPITAL_LETTER_A_WITH_STROKE, 'A')
      .replace(LATIN_CAPITAL_LETTER_C_WITH_STROKE, 'C')
      .replace(LATIN_CAPITAL_LETTER_L_WITH_BAR, 'L')
      .replace(LATIN_CAPITAL_LETTER_T_WITH_DIAGONAL_STROKE, 'T')
      .replace(LATIN_CAPITAL_LETTER_T_WITH_STROKE, 'T')
      .normalize('NFC');
  }
};

export default source;
