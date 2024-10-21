/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';
import { env } from 'node:process';

import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KeymanProjectCopier } from '../src/KeymanProjectCopier.js';
import { makePathToFixture } from './helpers/index.js';

const { TEST_SAVE_ARTIFACTS } = env;
let outputRoot: string = '/an/imaginary/root/';

describe('KeymanProjectCopier', function() {
  const callbacks = new TestCompilerCallbacks();

  this.beforeAll(function() {
    if(TEST_SAVE_ARTIFACTS) {
      outputRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'kmc-copy-'));
      console.log(`Output written to '${outputRoot}'`);
    }
  });

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  const tests = [

  //
  // kmhmu_2008
  //

  {
    title: 'it should run a v1.0 keyboard project (kmhmu_2008)',
    new: 'kmhmu_2024', old: 'kmhmu_2008',
    old_project: 'kmhmu_2008.kpj',
    expected_artifacts: {
      'kmhmu_2008.kpj': 'kmhmu_2024.kpj',
      'LICENSE.md': 'LICENSE.md',
      'source/kmhmu_2008.ico': 'source/kmhmu_2024.ico',
      'source/kmhmu_2008.kmn': 'source/kmhmu_2024.kmn',
      'source/kmhmu_2008.kps': 'source/kmhmu_2024.kps',
      'source/kmhmu_2008.kvks': 'source/kmhmu_2024.kvks',
      'source/readme.htm': 'source/readme.htm',
      'source/welcome/kmhmu_2008U_.png': 'source/welcome/kmhmu_2008U_.png',
      'source/welcome/kmhmu_2008U_RA.png': 'source/welcome/kmhmu_2008U_RA.png',
      'source/welcome/kmhmu_2008U_S.png': 'source/welcome/kmhmu_2008U_S.png',
      'source/welcome/welcome.htm': 'source/welcome/welcome.htm',
      'build/kmhmu_2008.kmx': 'build/kmhmu_2024.kmx',
      'build/kmhmu_2008.kvk': 'build/kmhmu_2024.kvk',
      'assets/Kmhmu_MX.TTF': 'assets/Kmhmu_MX.TTF',
      'assets/KmhmuMX_README.txt': 'assets/KmhmuMX_README.txt',
      'assets/OFL.txt': 'assets/OFL.txt',
      'assets/OFL-FAQ.txt': 'assets/OFL-FAQ.txt',
      // NOTE: the following files are not included because this is a 1.0
      // project and they are not referenced by any files:
      // * HISTORY.md
      // * README.md
      // * DEPRECATED.md
      // * source/help/*
      // * build/kmhmu_2008.kmp
      // * build/kmhmu_2008.keyboard_info
    },
    fixtures: {
      'kmhmu_2008.kpj': 'expected/kmhmu_2008/kmhmu_2024.kpj',
      'source/kmhmu_2008.kmn': 'expected/kmhmu_2008/kmhmu_2024.kmn',
      'source/kmhmu_2008.kps': 'expected/kmhmu_2008/kmhmu_2024.kps',
    }
  },

  //
  // khmer_angkor
  //

  {
    title: 'it should run a v2.0 keyboard project (khmer_angkor)',
    new: 'khmer_angkor_2024',
    old: 'khmer_angkor',
    old_project: 'khmer_angkor.kpj',
    expected_artifacts: {
      'khmer_angkor.kpj': 'khmer_angkor_2024.kpj',
      'LICENSE.md': 'LICENSE.md',
      'HISTORY.md': 'HISTORY.md',
      'README.md': 'README.md',

      'source/khmer_angkor.ico': 'source/khmer_angkor_2024.ico',
      'source/khmer_angkor.keyman-touch-layout': 'source/khmer_angkor_2024.keyman-touch-layout',
      'source/khmer_angkor.kmn': 'source/khmer_angkor_2024.kmn',
      'source/khmer_angkor.kps': 'source/khmer_angkor_2024.kps',
      'source/khmer_angkor.kvks': 'source/khmer_angkor_2024.kvks',
      'source/readme.htm': 'source/readme.htm',
      'source/splash.gif': 'source/splash.gif',

      'source/help/KAK_Documentation_EN.pdf': 'source/help/KAK_Documentation_EN.pdf',
      'source/help/KAK_Documentation_KH.pdf': 'source/help/KAK_Documentation_KH.pdf',
      'source/help/khmer_angkor.php': 'source/help/khmer_angkor_2024.php',

      'source/welcome/image002.png': 'source/welcome/image002.png',
      'source/welcome/KAK_Documentation_EN.pdf': 'source/welcome/KAK_Documentation_EN.pdf',
      'source/welcome/KAK_Documentation_KH.pdf': 'source/welcome/KAK_Documentation_KH.pdf',
      'source/welcome/keyboard_layout.png': 'source/welcome/keyboard_layout.png',
      'source/welcome/welcome.htm': 'source/welcome/welcome.htm',

      'build/khmer_angkor.js': 'build/khmer_angkor_2024.js',
      'build/khmer_angkor.kmx': 'build/khmer_angkor_2024.kmx',
      'build/khmer_angkor.kvk': 'build/khmer_angkor_2024.kvk',

      'extras/KAK_Documentation_EN.docx': 'extras/KAK_Documentation_EN.docx',
      'extras/regression_tests/1a.xml': 'extras/regression_tests/1a.xml',
      'extras/regression_tests/ស្ប៊ី.xml': 'extras/regression_tests/ស្ប៊ី.xml',
      'extras/regression_tests/ហ្វ៊ី.xml': 'extras/regression_tests/ហ្វ៊ី.xml',
      'extras/regression_tests/ignored/3a.xml': 'extras/regression_tests/ignored/3a.xml',

      // NOTE: the following files are explicitly ignored in the project folder copy:
      // * build/khmer_angkor.kmp
      // * build/khmer_angkor.keyboard_info
      // * khmer_angkor.kpj.user
    },
    fixtures: {
      'khmer_angkor.kpj': 'expected/khmer_angkor/khmer_angkor_2024.kpj',
      'source/khmer_angkor.kmn': (actual: string) => {
        // We need to strip out &DISPLAYMAP generated path because it will
        // vary depending on the file location
        const expected = fs.readFileSync(makePathToFixture('expected/khmer_angkor/khmer_angkor_2024.kmn'), 'utf-8');
        actual = actual.replace(/store\(&DISPLAYMAP\) '.+KbdKhmr.json'/, "store(&DISPLAYMAP) '.../KbdKhmr.json'");
        assert.equal(actual, expected);
      },
      'source/khmer_angkor.kps': (actual: string) => {
        // We need to strip out ../../.../kmc-copy/... generated path because it will
        // vary depending on the file location
        const expected = fs.readFileSync(makePathToFixture('expected/khmer_angkor/khmer_angkor_2024.kps'), 'utf-8');
        actual = actual.replace(/>.+kmc-copy\/test\/(.+)<\//g, '>.../$1</');
        assert.equal(actual, expected);
      },
    }
  },

  //
  // alephwithbeth
  //

  {
    title: 'should run on a v1.0 keyboard project with metadata (alephwithbeth)',
    new: 'alephbethnew',
    old: 'alephwithbeth',
    old_project: 'alephwithbeth.kpj',
    expected_artifacts: {
      "HISTORY.md": "HISTORY.md",
      "LICENSE.md": "LICENSE.md",
      "README.md": "README.md",
      "alephwithbeth.kpj": "alephbethnew.kpj",
      "build/alephwithbeth.js": "build/alephbethnew.js",
      "build/alephwithbeth.kmx": "build/alephbethnew.kmx",
      "build/alephwithbeth.kvk": "build/alephbethnew.kvk",
      "source/alephwithbeth.ico": "source/alephbethnew.ico",
      "source/alephwithbeth.keyman-touch-layout": "source/alephbethnew.keyman-touch-layout",
      "source/alephwithbeth.kmn": "source/alephbethnew.kmn",
      "source/alephwithbeth.kps": "source/alephbethnew.kps",
      "source/alephwithbeth.kvks": "source/alephbethnew.kvks",
      "source/readme.htm": "source/readme.htm",
      "source/welcome/KeyboardLayout.png": "source/welcome/KeyboardLayout.png"    ,
      "source/welcome/KeyboardLayoutAlt.png": "source/welcome/KeyboardLayoutAlt.png",
      "source/welcome/KeyboardLayoutShift.png": "source/welcome/KeyboardLayoutShift.png",
      "source/welcome/welcome.htm": "source/welcome/welcome.htm",
    },
    fixtures: {
      'alephwithbeth.kpj': 'expected/alephwithbeth/alephbethnew.kpj',
      'source/alephwithbeth.kmn': 'expected/alephwithbeth/alephbethnew.kmn',
      'source/alephwithbeth.kps': 'expected/alephwithbeth/alephbethnew.kps',
    }
  },

  //
  // gff.ti_er.gff_tigrinya_eritrea
  //

  {
    title: 'should run on a v1.0 model project (gff.ti_er.gff_tigrinya_eritrea)',
    new: 'sil.ti_er.my_version',
    old: 'gff.ti_er.gff_tigrinya_eritrea',
    old_project: 'gff.ti_er.gff_tigrinya_eritrea.kpj',
    expected_artifacts: {
      "HISTORY.md": "HISTORY.md",
      "LICENSE.md": "LICENSE.md",
      "README.md": "README.md",
      "build/gff.ti_er.gff_tigrinya_eritrea.model.js": "build/sil.ti_er.my_version.model.js",
      "gff.ti_er.gff_tigrinya_eritrea.kpj": "sil.ti_er.my_version.kpj",
      "gff.ti_er.gff_tigrinya_eritrea.model_info": "sil.ti_er.my_version.model_info",
      "source/TigrinyaErWordList.tsv": "source/TigrinyaErWordList.tsv",
      "source/gff.ti_er.gff_tigrinya_eritrea.model.kps": "source/sil.ti_er.my_version.model.kps",
      "source/gff.ti_er.gff_tigrinya_eritrea.model.ts": "source/sil.ti_er.my_version.model.ts",
      "source/readme.htm": "source/readme.htm",
      "source/welcome.htm": "source/welcome.htm",
    },
    fixtures: {
      'gff.ti_er.gff_tigrinya_eritrea.kpj': 'expected/gff.ti_er.gff_tigrinya_eritrea/sil.ti_er.my_version.kpj',
      'source/gff.ti_er.gff_tigrinya_eritrea.model.ts': 'expected/gff.ti_er.gff_tigrinya_eritrea/sil.ti_er.my_version.model.ts',
      'source/gff.ti_er.gff_tigrinya_eritrea.model.kps': 'expected/gff.ti_er.gff_tigrinya_eritrea/sil.ti_er.my_version.model.kps',
    }
  },

  //
  // burushos.bsk.burushaski
  //

  {
    title: 'should run on a v2.0 model project (burushos.bsk.burushaski)',
    new: 'sil.bsk.my_version',
    old: 'burushos.bsk.burushaski',
    old_project: 'burushos.bsk.burushaski.kpj',
    expected_artifacts: {
      "HISTORY.md": "HISTORY.md",
      "LICENSE.md": "LICENSE.md",
      "README.md": "README.md",
      "build/burushos.bsk.burushaski.model.js": "build/sil.bsk.my_version.model.js",
      "burushos.bsk.burushaski.kpj": "sil.bsk.my_version.kpj",
      "source/wordlist.tsv": "source/wordlist.tsv",
      "source/burushos.bsk.burushaski.model.kps": "source/sil.bsk.my_version.model.kps",
      "source/burushos.bsk.burushaski.model.ts": "source/sil.bsk.my_version.model.ts",
      "source/readme.htm": "source/readme.htm",
      "source/welcome.htm": "source/welcome.htm",
    },
    fixtures: {
      'burushos.bsk.burushaski.kpj': 'expected/burushos.bsk.burushaski/sil.bsk.my_version.kpj',
      'source/burushos.bsk.burushaski.model.ts': 'expected/burushos.bsk.burushaski/sil.bsk.my_version.model.ts',
      'source/burushos.bsk.burushaski.model.kps': 'expected/burushos.bsk.burushaski/sil.bsk.my_version.model.kps',
    }
  },
];

  for(const test of tests) {
    it(test.title, async function() {
      const outPath = path.join(outputRoot, test.new).replaceAll(/\\/g, '/');
      const source = makePathToFixture('projects', test.old).replaceAll(/\\/g, '/') + '/';

      const copier = new KeymanProjectCopier();
      assert.isTrue(await copier.init(callbacks, {
        dryRun: false,
        outPath,
        rename: false,
      }));

      const result = await copier.run(source + test.old_project);

      // We should have no messages and a successful result
      assert.isOk(result);
      assert.isEmpty(callbacks.messages);

      if(TEST_SAVE_ARTIFACTS) {
        assert.isTrue(await copier.write(result.artifacts));
      }

      // Verify that the special outputPath folder artifact is present

      assert.isNull(result.artifacts['kmc-copy:outputPath'].data);

      // Verify that the files are going into the right places

      // add paths in for comparison
      const expectedArtifacts = Object.keys(test.expected_artifacts).reduce(
        (obj,key) => { obj[source + key] = outPath + '/' + (<any>test.expected_artifacts)[key]; return obj},
        <any>{ "kmc-copy:outputPath": outPath }
      );

      // strip .data out for comparison
      const actualArtifacts = Object.keys(result.artifacts).reduce((obj,key) => { obj[key] = result.artifacts[key].filename; return obj }, <any>{});

      assert.deepEqual(actualArtifacts, expectedArtifacts);

      // Verify that rewritten files are correct
      for(const fixture of Object.keys(test.fixtures)) {
        const actual = new TextDecoder().decode(result.artifacts[source + fixture].data);

        if(typeof (<any>test.fixtures)[fixture] == 'function') {
          (<any>test.fixtures)[fixture](actual);
        } else {
          const expected = fs.readFileSync(makePathToFixture((<any>test.fixtures)[fixture]), 'utf-8');
          assert.equal(expected, actual);
        }
      }
    });
  }

  it.skip('should copy a disorganized project into current structure', async function() {});
  it.skip('should copy a standalone .kmn into a new project', async function() {});
  it.skip('should copy a standalone .kmn and .kps into a new project', async function() {});
});
