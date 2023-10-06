//@ts-expect-error
import kpj from './schemas/kpj.schema.validator.mjs';

//@ts-expect-error
import kpj90 from './schemas/kpj-9.0.schema.validator.mjs';

//@ts-expect-error
import kvks from './schemas/kvks.schema.validator.mjs';

//@ts-expect-error
import ldmlKeyboard3 from './schemas/ldml-keyboard3.schema.validator.mjs';

//@ts-expect-error
import ldmlKeyboardTest3 from './schemas/ldml-keyboardtest3.schema.validator.mjs';

//@ts-expect-error
import displayMap from './schemas/displaymap.schema.validator.mjs';

//@ts-expect-error
import touchLayoutClean from './schemas/keyman-touch-layout.clean.spec.validator.mjs';

//@ts-expect-error
import touchLayout from './schemas/keyman-touch-layout.spec.validator.mjs';

//@ts-expect-error
import keyboard_info from './schemas/keyboard_info.schema.validator.mjs';

// How to use: https://ajv.js.org/standalone.html#using-the-validation-function-s
// See also existing uses (search for `SchemaValidators`) for examples.
const SchemaValidators = {
  kpj,
  kpj90,
  kvks,
  ldmlKeyboard3,
  ldmlKeyboardTest3,
  displayMap,
  touchLayoutClean,
  touchLayout,
  keyboard_info,
};

export default SchemaValidators;
