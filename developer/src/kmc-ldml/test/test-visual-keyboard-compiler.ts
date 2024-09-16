/*
 * Keyman is copyright (C) SIL International. MIT License.
 */
import 'mocha';
import * as path from 'path';
import {assert} from 'chai';
import { stripIndent } from 'common-tags';

import { KMX, KvkFileWriter, LDMLKeyboardXMLSourceFileReader, VisualKeyboard } from '@keymanapp/common-types';
import hextobin from '@keymanapp/hextobin';

import { checkMessages, compilerTestCallbacks, compilerTestOptions, makePathToFixture } from './helpers/index.js';

import { LdmlKeyboardVisualKeyboardCompiler } from '../src/compiler/visual-keyboard-compiler.js';
import { LdmlKeyboardCompiler } from '../src/main.js';

describe('visual-keyboard-compiler', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should build fixtures', async function() {
    // Let's build basic.xml

    // It should match basic.kvk (built from basic-kvk.txt)
    const inputFilename = makePathToFixture('basic.xml');
    const binaryFilename = makePathToFixture('basic-kvk.txt');

    // Compile the visual keyboard
    const k = new LdmlKeyboardCompiler();
    assert.isTrue(await k.init(compilerTestCallbacks, {...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false}));
    const source = k.load(inputFilename);
    checkMessages();
    assert.isNotNull(source, 'k.load should not have returned null');

    const valid = await k.validate(source);
    checkMessages();
    assert.isTrue(valid, 'k.validate should not have failed');

    let kmx = await k.compile(source);
    assert(kmx, 'k.compile should not have failed');

    const keyboardId = path.basename(inputFilename, '.xml');

    const vk = (new LdmlKeyboardVisualKeyboardCompiler(compilerTestCallbacks)).compile(kmx.kmxplus, keyboardId);
    checkMessages();
    assert.isNotNull(vk, 'LdmlKeyboardVisualKeyboardCompiler.compile should not have returned null');
    assert.isNotNull(kmx.keyboard.stores.find(store =>
      store.dwSystemID == KMX.KMXFile.TSS_VISUALKEYBOARD &&
      store.dpString == keyboardId + '.kvk'
    ));
    assert(typeof vk == 'object');

    // Use the builder to generate the binary output file
    const writer = new KvkFileWriter();
    const code = writer.write(vk);
    assert.isEmpty(compilerTestCallbacks.messages);
    assert.isNotNull(code);

    // Compare output
    let expected = await hextobin(binaryFilename, undefined, {silent:true});

    assert.deepEqual<Uint8Array>(code, expected);
  });

  it('should support various modifiers', async function() {
    const xml = stripIndent`
      <?xml version="1.0" encoding="UTF-8"?>
      <keyboard3 xmlns="https://schemas.unicode.org/cldr/45/keyboard3" locale="mt" conformsTo="45">
        <info name="minimal"/>
        <keys>
          <key id="x" output="y" />
        </keys>
        <layers formId="us">
          <layer modifiers="none"><row keys="x" /></layer>
          <layer modifiers="shift"><row keys="x" /></layer>
          <layer modifiers="altR"><row keys="x" /></layer>
          <layer modifiers="ctrlR altR, shift altR"><row keys="x" /></layer>
          <layer modifiers="caps"><row keys="x" /></layer>
        </layers>
      </keyboard3>
    `;

    const vk = await loadVisualKeyboardFromXml(xml, 'test');

    assert.equal(vk.keys.length, 5);
    assert.equal(vk.keys[0].shift, VisualKeyboard.VisualKeyboardShiftState.KVKS_NORMAL);
    assert.equal(vk.keys[1].shift, VisualKeyboard.VisualKeyboardShiftState.KVKS_SHIFT);
    assert.equal(vk.keys[2].shift, VisualKeyboard.VisualKeyboardShiftState.KVKS_RALT);
    assert.equal(vk.keys[3].shift, VisualKeyboard.VisualKeyboardShiftState.KVKS_RCTRL | VisualKeyboard.VisualKeyboardShiftState.KVKS_RALT);
    assert.equal(vk.keys[4].shift, VisualKeyboard.VisualKeyboardShiftState.KVKS_SHIFT | VisualKeyboard.VisualKeyboardShiftState.KVKS_RALT);
  });

  it('should emit the correct associated keyboard id', async function() {
    const xml = stripIndent`
      <?xml version="1.0" encoding="UTF-8"?>
      <keyboard3 xmlns="https://schemas.unicode.org/cldr/45/keyboard3" locale="mt" conformsTo="45">
        <info name="minimal"/>
        <keys>
          <key id="x" output="y" />
        </keys>
        <layers formId="us">
          <layer modifiers="none"><row keys="x" /></layer>
        </layers>
      </keyboard3>
    `;

    const vk = await loadVisualKeyboardFromXml(xml, 'test');

    assert.equal(vk.header.associatedKeyboard, 'test');
  });

  it('should support <display>', async function() {
    const xml = stripIndent`
      <?xml version="1.0" encoding="UTF-8"?>
      <keyboard3 xmlns="https://schemas.unicode.org/cldr/45/keyboard3" locale="mt" conformsTo="45">
        <info name="minimal"/>
        <keys>
          <key id="a" output="X" />
          <key id="b" output="Y" />
        </keys>
        <displays>
          <display keyId="a" display="C" />
          <display output="Y" display="D" />
        </displays>
        <layers formId="us">
          <layer modifiers="none"><row keys="a b" /></layer>
        </layers>
      </keyboard3>
    `;

    const vk = await loadVisualKeyboardFromXml(xml, 'test');

    assert.equal(vk.keys.length, 2);
    assert.equal(vk.keys[0].text, 'C');
    assert.equal(vk.keys[1].text, 'D');
  });

  it('should correctly decode \\u{xxxx}', async function() {
    const xml = stripIndent`
      <?xml version="1.0" encoding="UTF-8"?>
      <keyboard3 xmlns="https://schemas.unicode.org/cldr/45/keyboard3" locale="mt" conformsTo="45">
        <info name="minimal"/>
        <keys>
          <key id="x" output="\\u{0e80}" />
          <key id="z" output="y" />
        </keys>
        <displays>
          <display output="y" display="\\u{0e81}" />
        </displays>
        <layers formId="us">
          <layer modifiers="none"><row keys="x z" /></layer>
        </layers>
      </keyboard3>
    `;

    const vk = await loadVisualKeyboardFromXml(xml, 'test');

    assert.equal(vk.keys.length, 2);
    assert.equal(vk.keys[0].text, '\u{0e80}');
    assert.equal(vk.keys[1].text, '\u{0e81}');
  });

  it('should read string variables in key.output', async function() {
    const xml = stripIndent`
      <?xml version="1.0" encoding="UTF-8"?>
      <keyboard3 xmlns="https://schemas.unicode.org/cldr/45/keyboard3" locale="mt" conformsTo="45">
        <info name="minimal"/>
        <keys>
          <key id="x" output="\${one}" />
        </keys>
        <layers formId="us">
          <layer modifiers="none"><row keys="x" /></layer>
        </layers>
        <variables>
          <string id="two" value="2" />
          <string id="one" value="\${two}" />
        </variables>
      </keyboard3>
    `;

    const vk = await loadVisualKeyboardFromXml(xml, 'test');

    assert.equal(vk.keys.length, 1);
    assert.equal(vk.keys[0].text, '2');
  });

  it('should read string variables in display.display', async function() {
    const xml = stripIndent`
      <?xml version="1.0" encoding="UTF-8"?>
      <keyboard3 xmlns="https://schemas.unicode.org/cldr/45/keyboard3" locale="mt" conformsTo="45">
        <info name="minimal"/>
        <keys>
          <key id="x" output="y" />
        </keys>
        <displays>
          <display output="y" display="\${one}" />
        </displays>
        <layers formId="us">
          <layer modifiers="none"><row keys="x" /></layer>
        </layers>
        <variables>
          <string id="two" value="2" />
          <string id="one" value="\${two}" />
        </variables>
      </keyboard3>
    `;

    const vk = await loadVisualKeyboardFromXml(xml, 'test');

    assert.equal(vk.keys.length, 1);
    assert.equal(vk.keys[0].text, '2');
  });
});

async function loadVisualKeyboardFromXml(xml: string, id: string) {
  const data = new TextEncoder().encode(xml);
  assert.isOk(data);

  const reader = new LDMLKeyboardXMLSourceFileReader(compilerTestOptions.readerOptions, compilerTestCallbacks);
  const source = reader.load(data);
  assert.isEmpty(compilerTestCallbacks.messages);
  assert.isOk(source);

  const k = new LdmlKeyboardCompiler();
  assert.isTrue(await k.init(compilerTestCallbacks, compilerTestOptions));

  const kmx = await k.compile(source);
  assert(kmx, 'k.compile should not have failed');

  const vk = (new LdmlKeyboardVisualKeyboardCompiler(compilerTestCallbacks)).compile(kmx.kmxplus, id);
  assert(typeof vk == 'object');
  assert.isEmpty(compilerTestCallbacks.messages);
  assert.isOk(vk);

  return vk;
}
