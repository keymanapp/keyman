/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2024-10-16
 */

import * as fs from 'fs';
import 'mocha';
import {assert} from 'chai';

import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

import { makePathToFixture } from '../helpers/index.js';

import { KpsFileReader } from "../../src/types/kps/kps-file-reader.js";
import { KpsFileWriter } from '../../src/types/kps/kps-file-writer.js';
import { CommonTypesMessages } from '../../src/common-messages.js';

const callbacks = new TestCompilerCallbacks();

describe('kps-file-reader', function () {
  it('kps-file-reader should read a valid file', function() {
    const input = fs.readFileSync(makePathToFixture('kps', 'khmer_angkor.kps'));
    const reader = new KpsFileReader(callbacks);
    const kps = reader.read(input);

    assert.equal(kps.Package.System.FileVersion, '7.0');
    assert.equal(kps.Package.System.KeymanDeveloperVersion, '15.0.266.0');
    assert.equal(kps.Package.Info.Author._, 'Makara Sok');
    assert.equal(kps.Package.Info.Author.$.URL, 'mailto:makara_sok@sil.org');
    assert.equal(kps.Package.Info.Copyright._, '© 2015-2022 SIL International');
    assert.isUndefined(kps.Package.Info.Description);
    assert.equal(kps.Package.Info.Name._, 'Khmer Angkor');
    assert.isUndefined(kps.Package.Info.Version._);
    assert.equal(kps.Package.Info.WebSite._, 'https://keyman.com/keyboards/khmer_angkor');
    assert.equal(kps.Package.Info.WebSite.$.URL, 'https://keyman.com/keyboards/khmer_angkor');

    assert.isUndefined(kps.Package.Options.LicenseFile);
    assert.equal(kps.Package.Options.FollowKeyboardVersion, '');
    assert.equal(kps.Package.Options.ReadMeFile, 'readme.htm');
    assert.equal(kps.Package.Options.WelcomeFile, 'welcome.htm');
    assert.equal(kps.Package.Options.GraphicFile, 'splash.gif');

    assert.lengthOf(kps.Package.Files.File, 15);
    assert.equal(kps.Package.Files.File[0].Name, '..\\build\\khmer_angkor.js');
    assert.equal(kps.Package.Files.File[1].Name, '..\\build\\khmer_angkor.kvk');

    assert.lengthOf(kps.Package.Keyboards.Keyboard, 1);
    assert.equal(kps.Package.Keyboards.Keyboard[0].ID, 'khmer_angkor');
    assert.equal(kps.Package.Keyboards.Keyboard[0].Name, 'Khmer Angkor');
    assert.equal(kps.Package.Keyboards.Keyboard[0].Version, '1.3');

    assert.lengthOf(kps.Package.Keyboards.Keyboard[0].Languages.Language, 1);
    assert.equal(kps.Package.Keyboards.Keyboard[0].Languages.Language[0]._, 'Central Khmer (Khmer, Cambodia)');
    assert.equal(kps.Package.Keyboards.Keyboard[0].Languages.Language[0].$.ID, 'km');

    assert.equal(kps.Package.Keyboards.Keyboard[0].OSKFont, '..\\shared\\fonts\\khmer\\busrakbd\\khmer_busra_kbd.ttf');
    assert.equal(kps.Package.Keyboards.Keyboard[0].DisplayFont, '..\\shared\\fonts\\khmer\\mondulkiri\\Mondulkiri-R.ttf');
  });

  it('kps-file-reader should round-trip with kps-file-writer', function() {
    const input = fs.readFileSync(makePathToFixture('kps', 'khmer_angkor.kps'));
    const reader = new KpsFileReader(callbacks);
    const kps = reader.read(input);

    const writer = new KpsFileWriter();
    const output = writer.write(kps);

    // Round Trip
    const kps2 = reader.read(new TextEncoder().encode(output));
    assert.deepEqual(kps2, kps);
  });

  // ERROR_InvalidPackageFile

  it('should generate ERROR_InvalidPackageFile if package source file contains invalid XML', async function() {
    const input = fs.readFileSync(makePathToFixture('kps', 'error_invalid_package_file.kps'));
    const reader = new KpsFileReader(callbacks);
    const kps = reader.read(input);

    assert.isNull(kps);
    assert.lengthOf(callbacks.messages, 1);
    assert.isTrue(callbacks.hasMessage(CommonTypesMessages.ERROR_InvalidPackageFile));
  });
});
