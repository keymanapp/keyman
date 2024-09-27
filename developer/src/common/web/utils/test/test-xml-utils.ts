import { assert } from 'chai';
import 'mocha';

import { KeymanXMLParser, KeymanXMLGenerator } from '../src/xml-utils.js';

describe('XML Parser Test', () => {
  it('null test', () => assert.ok(new KeymanXMLParser()));
});

describe('XML Generator Test', () => {
  it('null test', () => assert.ok(new KeymanXMLGenerator()));
});

