/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
async function loadKeyboards() {
  const keyboardList = [
    { id: 'devanagari_inscript', name: 'Devanagari (INSCRIPT)', languages: { id: 'hi', name: 'Hindi' }, filename: './js/devanagari_inscript.js' },
    { id: 'european2', name: 'EuroLatin2', languages: { id: 'en', name: 'English' }, filename: './js/european2.js' },
    { id: 'hebrew', name: 'Hebrew', languages: { id: 'he', name: 'Hebrew' }, filename: './js/hebrew.js' },
    { id: 'korean_korda', name: 'Korean (KORDA)', languages: { id: 'ko', name: 'Korean' }, filename: './js/korean_korda.js' },
    { id: 'korean_morse', name: 'Korean (Morse)', languages: { id: 'ko', name: 'Korean' }, filename: './js/korean_morse.js' },
    // using a KeyboardStub would allow to customize e.g. which font the keyboard uses
    { KF: "./js/laokeys.js", KI: "Keyboard_laokeys", KN: "Lao (Phonetic)", KL: "Lao", KLC: "lo" }
  ];

  return keyman.addKeyboards(keyboardList).catch(function(err) {
    console.error('keyman.addKeyboards failed with '+errToString(err)+' for '+JSON.stringify(keyboardList));
  });
}
