import {assert} from 'chai';
import 'mocha';
import { extractVersionData } from '../src/version-data.js';

describe('extractVersionData', function() {
  it('should parse version strings', function() {
    let v = extractVersionData('14.0.283');
    assert.equal(v.version, '14.0.283');
    assert.equal(v.versionRelease, '14.0');
    assert.equal(v.versionMajor, '14');
    assert.equal(v.versionMinor, '0');
    assert.equal(v.versionPatch, '283');
    assert.equal(v.tier, 'stable');
    assert.equal(v.versionTag, '');
    assert.equal(v.versionWithTag, '14.0.283');
    assert.equal(v.versionEnvironment, 'stable');
    assert.equal(v.pr, '');

    v = extractVersionData('14.0.283-beta');
    assert.equal(v.version, '14.0.283');
    assert.equal(v.versionRelease, '14.0');
    assert.equal(v.versionMajor, '14');
    assert.equal(v.versionMinor, '0');
    assert.equal(v.versionPatch, '283');
    assert.equal(v.tier, 'beta');
    assert.equal(v.versionTag, '-beta');
    assert.equal(v.versionWithTag, '14.0.283-beta');
    assert.equal(v.versionEnvironment, 'beta');
    assert.equal(v.pr, '');

    v = extractVersionData('14.0.283-alpha-local');
    assert.equal(v.version, '14.0.283');
    assert.equal(v.versionRelease, '14.0');
    assert.equal(v.versionMajor, '14');
    assert.equal(v.versionMinor, '0');
    assert.equal(v.versionPatch, '283');
    assert.equal(v.tier, 'alpha');
    assert.equal(v.versionTag, '-alpha-local');
    assert.equal(v.versionWithTag, '14.0.283-alpha-local');
    assert.equal(v.versionEnvironment, 'local');
    assert.equal(v.pr, '');

    v = extractVersionData('14.0.283-alpha-test-1234');
    assert.equal(v.version, '14.0.283');
    assert.equal(v.versionRelease, '14.0');
    assert.equal(v.versionMajor, '14');
    assert.equal(v.versionMinor, '0');
    assert.equal(v.versionPatch, '283');
    assert.equal(v.tier, 'alpha');
    assert.equal(v.versionTag, '-alpha-test-1234');
    assert.equal(v.versionWithTag, '14.0.283-alpha-test-1234');
    assert.equal(v.versionEnvironment, 'test');
    assert.equal(v.pr, '1234');
  });
});