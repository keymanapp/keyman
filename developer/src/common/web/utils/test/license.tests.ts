import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers/index.js';
import { validateMITLicense } from '../src/utils/validate-mit-license.js';

function verifyLicenseFile(filename: string) {
  return validateMITLicense(fs.readFileSync(makePathToFixture('license', filename), 'utf-8'));
}

describe('validate-mit-license', function () {
  it('should accept a valid license', function() {
    assert.isNull(verifyLicenseFile('LICENSE.md'));

  });
  it('should catch invalid licenses', function() {
    assert.equal(verifyLicenseFile('LICENSE-changed-clause.md'), 'Clause 3 differs from MIT license');
    assert.equal(verifyLicenseFile('LICENSE-missing-copyright.md'), "Clause 2 does not start with 'Copyright'");
    assert.equal(verifyLicenseFile('LICENSE-missing-title.md'), 'Clause 1 differs from MIT license');
    assert.equal(verifyLicenseFile('LICENSE-too-long.md'), 'License contains extra text');
    assert.equal(verifyLicenseFile('LICENSE-too-short.md'), 'License is missing clauses from MIT license');
  });
});
