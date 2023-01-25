// Compiles completely out if `const enum`, making it unavailable in JS-based unit tests.
enum SpacebarText {
  KEYBOARD = 'keyboard',
  LANGUAGE = 'language',
  LANGUAGE_KEYBOARD = 'languageKeyboard',
  BLANK = 'blank'
};

export default SpacebarText;