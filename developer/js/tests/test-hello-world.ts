import LexicalModelCompiler from '../';
import {assert} from 'chai';
import 'mocha';

describe('LexicalModelCompiler', function () {
  describe('#hello()', function () {
    it('should say "hello"', function () {
      assert.strictEqual((new LexicalModelCompiler).hello(), 'Hello, world!');
    });
  });
});
