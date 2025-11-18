import * as fs from 'node:fs';
import 'mocha';
import {assert} from 'chai';
import hextobin from '@keymanapp/hextobin';
import {compileKeyboard, compilerTestCallbacks, compilerTestOptions, makePathToFixture, scrubContextFromMessages} from './helpers/index.js';
import { compareXml } from './helpers/compareXml.js';
import { LdmlKeyboardCompiler } from '../src/compiler/compiler.js';
import { kmxToXml } from '../src/util/serialize.js';
import { writeFileSync } from 'node:fs';
import { LdmlCompilerMessages } from '../src/main.js';
import { KMX, util } from '@keymanapp/common-types';
import { KMXPlusBuilder, SectionBuilders } from '@keymanapp/developer-utils';
import { constants, KMXPlusVersion } from '@keymanapp/ldml-keyboard-constants';

const debug=false;

/** Overall compiler tests */
describe('compiler-tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  beforeEach(function() {
    compilerTestCallbacks.clear();
  });

  afterEach(function() {
    if (this.currentTest.state !== 'passed') {
      compilerTestCallbacks.printMessages();
    }
  });

  [
    [17, KMX.KMX_Version.VERSION_170],
    [19, KMX.KMX_Version.VERSION_190],
  ].forEach(([vernum, version]) => {
    it(`should-build-fixtures for v${vernum}.0`, async function() {
      this.timeout(4000);
      // Let's build basic.xml
      // It should match basic.kmx (built from basic.txt)

      const inputFilename = makePathToFixture('basic.xml');
      const binaryFilename = makePathToFixture(`basic-${vernum}.txt`);

      // Compare output
      const expected = await hextobin(binaryFilename, undefined, {silent:true});

      // now compare it to use with run()
      // Let's build basic.xml
      // It should match basic-vv.kmx (built from basic.txt)
      const k = new LdmlKeyboardCompiler();
      await k.init(compilerTestCallbacks, { ...compilerTestOptions, targetVersion: version, saveDebug: true, shouldAddCompilerVersion: false });

      const { artifacts } = await k.run(inputFilename, "basic-xml.kmx"); // need the exact name passed to build-fixtures
      assert.isNotNull(artifacts);
      const { kmx, kvk } = artifacts;
      assert.isNotNull(kmx);
      assert.isNotNull(kmx.data);
      if(debug) {
        fs.writeFileSync(makePathToFixture(`basic-${vernum}-actual.kmx`), kmx.data);
        fs.writeFileSync(makePathToFixture(`basic-${vernum}-expected.kmx`), expected);
      }
      assert.deepEqual<Uint8Array>(kmx.data, expected);

      // TODO-LDML: compare the .kvk file to something else?
      assert.isNotNull(kvk?.data);
    });
  });

  it('should not build a v19 file with incorrect section versions for sect, disp, and layr', async function() {
      const inputFilename = makePathToFixture('basic.xml');
      const kmxPlusBuilder = await runKmxPlusCompiler(inputFilename, KMX.KMX_Version.VERSION_190);

      assert.equal(kmxPlusBuilder.sect.sect?.header.ident, constants.sectionid_sec2);

      // verify sections that should be v19 - sect (aka sec2), disp, and layr
      assertSectionVersions(
        kmxPlusBuilder.sect,
        ['sect','disp','layr'],
        KMXPlusVersion.Version19
      );

      // verify sections that should be v17 - all should be present
      assertSectionVersions(
        kmxPlusBuilder.sect,
        ['bksp','elem','keys','list','loca','meta','strs','tran','uset','vars'],
        KMXPlusVersion.Version17
      );
  });

  it('should not build a v17 file with incorrect section versions', async function() {
      const inputFilename = makePathToFixture('basic.xml');
      const kmxPlusBuilder = await runKmxPlusCompiler(inputFilename, KMX.KMX_Version.VERSION_170);

      assert.equal(kmxPlusBuilder.sect.sect?.header.ident, constants.hex_section_id(constants.section.sect));

      assertSectionVersions(
        kmxPlusBuilder.sect,
        ['bksp','disp','elem','keys','layr','list','loca','meta','sect','strs','tran','uset','vars'],
        KMXPlusVersion.Version17
      );
  });

  // Helper functions

  function assertSectionVersions(sect: SectionBuilders, idents: (keyof SectionBuilders)[], version: KMXPlusVersion) {
    idents.forEach(ident => {
      assert.isNotNull(sect[ident]);
      assert.isNotNull(sect[ident].header);
      assert.equal(sect[ident].header.version, version);
    });
  }

  async function runKmxPlusCompiler(inputFilename: string, version: KMX.KMX_Version) {
    const compiler = new LdmlKeyboardCompiler();
    assert.isTrue(await compiler.init(compilerTestCallbacks, { ...compilerTestOptions, targetVersion: version }));
    const source = compiler.load(inputFilename);
    assert.isNotNull(source);
    const kmxPlusFile = await compiler.compile(source, true);
    assert.isNotNull(kmxPlusFile);

    const kmxPlusBuilder = new KMXPlusBuilder(kmxPlusFile);
    assert.isNotNull(kmxPlusBuilder.compile());
    return kmxPlusBuilder;
  }


  it('should-validate-on-run compiling sections/strs/invalid-illegal.xml', async function() {
    this.timeout(4000);
    const inputFilename = makePathToFixture('sections/strs/invalid-illegal.xml');

    // should fail validation
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });

    const runOutput = await k.run(inputFilename, "invalid-illegal.kmx"); // need the exact name passed to build-fixtures
    assert.isNull(runOutput, "Expect invalid-illegal to fail to run()");
    assert.sameDeepMembers( scrubContextFromMessages(compilerTestCallbacks.messages), [
      // copied from strs.tests.ts
      // validation messages
      LdmlCompilerMessages.Error_IllegalCharacters({ count: 5, lowestCh: util.describeCodepoint(0xFDD0) }),
      LdmlCompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: util.describeCodepoint(0xE010) }),
    ]);
  });


  it('should-serialize-kmx', async function() {
    this.timeout(4000);
    // Let's build basic.xml
    // It should match basic.kmx (built from basic.txt)
    const inputFilename = makePathToFixture('basic.xml');

    // Compile the keyboard
    const kmx = await compileKeyboard(inputFilename, {...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false});
    assert.isNotNull(kmx);

    // now output it as XML
    const outputFilename = makePathToFixture('basic-serialized.xml');
    const asXml = kmxToXml(kmx);
    writeFileSync(outputFilename, asXml, 'utf-8');

    compareXml(outputFilename, inputFilename);
  });

  it('should handle non existent files', async () => {
    const filename = 'DOES_NOT_EXIST.xml';
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle unparseable files', async () => {
    const filename = makePathToFixture('basic-kvk.txt'); // not an .xml file
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle not-valid files', async () => {
    const filename = makePathToFixture('test-fr.xml'); // not a keyboard .xml file
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle non existent test files', async () => {
    const filename = 'DOES_NOT_EXIST.xml';
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.loadTestData(filename);
    assert.notOk(source, `Trying to loadTestData(${filename})`);
  });
  it('should handle unparseable test files', async () => {
    const filename = makePathToFixture('basic-kvk.txt'); // not an .xml file
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to loadTestData(${filename})`);
  });
});
