function loadKeyboards()
{
  var kmw=keyman;

  // Add more keyboards to the language menu, by keyboard name,
  // keyboard name and language code, or just the BCP-47 language code.
  // We use a different loading pattern here than in the samples version to provide a slightly different set of test cases.
  kmw.addKeyboards('french','@he');
  kmw.addKeyboards({id:'sil_euro_latin', name:'SIL EuroLatin', languages: [{id:'no'}, {id:'sv'}]}); // Loads from partial stub instead of the compact string.

  // Add a keyboard by language name.  Note that the name must be spelled
  // correctly, or the keyboard will not be found.  (Using BCP-47 codes is
  // usually easier.)
  kmw.addKeyboardsForLanguage('Dzongkha');
}