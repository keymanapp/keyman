/**
 * Smoke-test the default
 */

import { assert } from 'chai';
import { searchForProperty } from '../build/obj/default/searchForProperty.js';
import { propertyMap } from '../build/obj/default/data.inc.js';

describe('searchForProperty', () => {
  it('correctly finds character classes for standard ASCII characters', () => {
    assert.equal(searchForProperty('a'.codePointAt(0)), propertyMap.indexOf('ALetter'));
    assert.equal(searchForProperty('Z'.codePointAt(0)), propertyMap.indexOf('ALetter'));

    assert.equal(searchForProperty("'".codePointAt(0)), propertyMap.indexOf('Single_Quote'));
    assert.equal(searchForProperty('"'.codePointAt(0)), propertyMap.indexOf('Double_Quote'));
    assert.equal(searchForProperty(','.codePointAt(0)), propertyMap.indexOf('MidNum'));
    assert.equal(searchForProperty('.'.codePointAt(0)), propertyMap.indexOf('MidNumLet'));
    assert.equal(searchForProperty('-'.codePointAt(0)), propertyMap.indexOf('Other'));
  });

  it('correctly finds character classes for specialized BMP characters', () => {
    assert.equal(searchForProperty(0x05D0), propertyMap.indexOf('Hebrew_Letter'));
    assert.equal(searchForProperty(0x3031), propertyMap.indexOf('Katakana'));
    assert.equal(searchForProperty(0xFFFE), propertyMap.indexOf('Other'));
    assert.equal(searchForProperty(0xFFFF), propertyMap.indexOf('Other'));
  });

  it('correctly finds character classes for non-BMP characters', () => {
    assert.equal(searchForProperty(0x0001F1E6), propertyMap.indexOf('Regional_Indicator'));
    assert.equal(searchForProperty(0x00013430), propertyMap.indexOf('Format'));
    assert.equal(searchForProperty(0x00010000), propertyMap.indexOf('ALetter'));
  });
});