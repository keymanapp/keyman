
// @ts-expect-error(2821) import assertions
import kpj from './schemas/kpj.schema.json' assert { type: 'json' };
// @ts-expect-error(2821) import assertions
import kpj90 from './schemas/kpj-9.0.schema.json' assert { type: 'json' };
// @ts-expect-error(2821) import assertions
import kvks from './schemas/kvks.schema.json' assert { type: 'json' };
// @ts-expect-error(2821) import assertions
import ldmlKeyboard from './schemas/ldml-keyboard.schema.json' assert { type: 'json' };
// @ts-expect-error(2821) import assertions
import ldmlKeyboardTest from './schemas/ldml-keyboardtest.schema.json' assert { type: 'json' };
// @ts-expect-error(2821) import assertions
import displayMap from './schemas/displaymap.schema.json' assert { type: 'json' };
// @ts-expect-error(2821) import assertions
import touchLayoutClean from './schemas/keyman-touch-layout.clean.spec.json' assert { type: 'json' };
// @ts-expect-error(2821) import assertions
import touchLayout from './schemas/keyman-touch-layout.spec.json' assert { type: 'json' };

const Schemas = {
  kpj,
  kpj90,
  kvks,
  ldmlKeyboard,
  ldmlKeyboardTest,
  displayMap,
  touchLayoutClean,
  touchLayout,
};

export default Schemas;