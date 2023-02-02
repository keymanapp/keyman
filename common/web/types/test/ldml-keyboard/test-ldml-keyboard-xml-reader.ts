import 'mocha';
import { CommonTypesMessages } from '../../src/util/common-events.js';
import { testReaderCases } from '../helpers/reader-callback-test.js';

describe('ldml keyboard xml reader tests', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  testReaderCases([
    {
      subpath: 'invalid-structure-per-dtd.xml',
      errors: [CommonTypesMessages.Fatal_AJVError({
        instancePath: '/keyboard',
        keyword: 'required',
        message: `must have required property 'names'`,
        schemaPath: null,
        params: { missingProperty: 'names' }
      })],
    },
    {
      subpath: 'invalid-conforms-to.xml',
      errors: [CommonTypesMessages.Fatal_AJVError({
        instancePath: '/keyboard/conformsTo',
        keyword: 'enum',
        message: `must be equal to one of the allowed values`,
        schemaPath: null,
        params: { allowedValues: ['techpreview'] }
      })],
    },
    {
      subpath: 'import-minimal.xml',
      // TODO-LDML: validate content
    },
    {
      subpath: 'import-minimal1.xml',
      // TODO-LDML: validate content
    },
    {
      subpath: 'import-minimal2.xml',
      // TODO-LDML: validate content
    },
    {
      subpath: 'import-symbols.xml',
      // TODO-LDML: validate content
    },
    {
      subpath: 'invalid-import-base.xml',
      errors: [
        CommonTypesMessages.Error_ImportInvalidBase({
          base: 'SOME_INVALID_BASE',
          path: 'B',
          subtag: 'C'
        }),
      ],
    },
    {
      subpath: 'invalid-import-path.xml',
      errors: [
        CommonTypesMessages.Error_ImportInvalidPath({
          base: null,
          path: 'techpreview/too/many/slashes/leading/to/nothing-Zxxx-does-not-exist.xml',
          subtag: null,
        }),
      ],
    },
    {
      subpath: 'invalid-import-readfail.xml',
      errors: [
        CommonTypesMessages.Error_ImportReadFail({
          base: null,
          path: 'techpreview/none-Zxxx-does-not-exist.xml',
          subtag: null,
        }),
      ],
    },
    {
      subpath: 'invalid-import-wrongroot.xml',
      errors: [
        CommonTypesMessages.Error_ImportWrongRoot({
          base: null,
          path: 'techpreview/keys-Zyyy-punctuation.xml',
          subtag: 'names',
        }),
      ],
    },
  ]);
});
