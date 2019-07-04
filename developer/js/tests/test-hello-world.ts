import LexicalModelCompiler from '../';
import {expect} from 'chai';
import 'mocha';

describe('LexicalModelCompiler', function () {
  describe('#hello()', function () {
    it('should say "hello"', function () {
      expect((new LexicalModelCompiler).hello()).to.be('Hello, world!');
    });
  });
});
