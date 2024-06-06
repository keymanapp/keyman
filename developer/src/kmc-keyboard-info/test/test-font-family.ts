import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers/index.js';
import { getFontFamily } from '../src/font-family.js';

const AFGHAN_TURKMEN_DISPLAY_FONT = makePathToFixture('afghan_turkmen', "Lateef-Regular.ttf");
const AFGHAN_TURKMEN_OSK_FONT = makePathToFixture('afghan_turkmen', "Lateef-Bold.ttf");

const AFGHAN_TURKMEN_DISPLAY_FACE_NAME = "Lateef";
const AFGHAN_TURKMEN_OSK_FACE_NAME = "Lateef";

describe('font-family', function () {
  it('correctly reads font facename from TrueType font file', async function() {
    // #11625
    const displayFontData: Uint8Array = fs.readFileSync(AFGHAN_TURKMEN_DISPLAY_FONT);
    const displayFacename = await getFontFamily(displayFontData);
    assert.equal(displayFacename, AFGHAN_TURKMEN_DISPLAY_FACE_NAME);

    const oskFontData: Uint8Array = fs.readFileSync(AFGHAN_TURKMEN_OSK_FONT);
    const oskFacename = await getFontFamily(oskFontData);
    assert.equal(oskFacename, AFGHAN_TURKMEN_OSK_FACE_NAME);
  });

  it.skip('can read all font files in keyboards repo', async function() {
    this.timeout(100000);
    async function testFonts(path: string) {
      const files = fs.readdirSync(path);
      for(const file of files) {
        if(fs.statSync(path + file).isDirectory()) {
          await testFonts(path + file + '/');
        } else if(file.match(/\.(ttf|otf)$/i)) {
          await testFont(path + file);
        }
      }
    }

    async function testFont(file: string) {
      console.log(`Testing ${file}`);
      const fontData: Uint8Array = fs.readFileSync(file);
      const facename = await getFontFamily(fontData);
      assert.isNotEmpty(facename);
      assert.isFalse(facename.includes('\u0000'));
    }

    // To enable this, we need to have access to the shared fonts in the
    // keyboards repo
    await testFonts('.../keyboards/release/shared/fonts/');
  });
});
