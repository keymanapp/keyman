import 'mocha';
import * as fs from 'fs';
import { assert } from 'chai';
import { makePathToFixture } from './helpers/index.js';
import { getCompiledWebKeyboardMetadata } from '../src/compiler/web-keyboard-metadata.js';

describe('web-keyboard-metadata', function () {
  it('should parse a minimized web keyboard correctly', function () {
    const jsFilePath = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.js');
    const js = fs.readFileSync(jsFilePath, 'utf-8')

    const metadata = getCompiledWebKeyboardMetadata(js);
    assert.equal('Khmer Angkor', metadata.keyboardName);
    assert.equal('1.3', metadata.keyboardVersion);
    assert.equal('10.0', metadata.minKeymanVersion);
    assert.equal(false, metadata.isMnemonic);
    assert.equal(false, metadata.isRtl);
  });

  it('should parse a debug-build web keyboard correctly', function () {
    const jsFilePath = makePathToFixture('khmer_angkor-debug-build.js');
    const js = fs.readFileSync(jsFilePath, 'utf-8')

    const metadata = getCompiledWebKeyboardMetadata(js);
    assert.equal('Khmer Angkor', metadata.keyboardName);
    assert.equal('1.0.3', metadata.keyboardVersion);
    assert.equal('10.0', metadata.minKeymanVersion);
    assert.equal(false, metadata.isMnemonic);
    assert.equal(false, metadata.isRtl);
  });
});
