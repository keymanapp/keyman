import 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import { assert } from 'chai';
import JSZip from 'jszip';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { WindowsPackageInstallerCompiler, WindowsPackageInstallerSources } from '../src/compiler/windows-package-installer-compiler.js';

describe('WindowsPackageInstallerCompiler', function () {
  it(`should build an SFX archive`, async function () {
    this.timeout(10000); // this test can take a little while to run

    const kpsPath = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const sources: WindowsPackageInstallerSources = {
      licenseFilename: makePathToFixture('windows-installer', 'license.txt'),
      msiFilename: makePathToFixture('windows-installer', 'keymandesktop.txt'),
      setupExeFilename: makePathToFixture('windows-installer', 'setup.txt'),
      startDisabled: false,
      startWithConfiguration: true,
      appName: 'Testing',
    };

    const callbacks = new TestCompilerCallbacks();
    let compiler = new WindowsPackageInstallerCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));

    const result = await compiler.run(kpsPath, null);
    assert.isNotNull(result);

    // This returns a buffer with a SFX loader and a zip suffix. For the sake of repository size
    // we actually provide a stub SFX loader and a stub MSI file, which is enough to verify that
    // the compiler is generating what it thinks is a valid file.

    const sfxBuffer = result.artifacts.exe.data;

    const zip = JSZip();

    // Check that file.kmp contains just 3 files - setup.inf, keymandesktop.msi, and khmer_angkor.kmp,
    // and that they match exactly what we expect
    const setupExeSize = fs.statSync(sources.setupExeFilename).size;
    const setupExe = sfxBuffer.slice(0, setupExeSize);
    const zipBuffer = sfxBuffer.slice(setupExeSize);

    // Verify setup.exe sfx loader
    const setupExeFixture = fs.readFileSync(sources.setupExeFilename);
    assert.deepEqual(setupExe, setupExeFixture);

    // Load the zip from the buffer
    const zipFile = await zip.loadAsync(zipBuffer, {checkCRC32: true});

    // Verify setup.inf; note that BitmapFileName splash.gif comes from the .kmp
    const setupInfFixture = `[Setup]
Version=${KEYMAN_VERSION.VERSION}
MSIFileName=${path.basename(sources.msiFilename)}
MSIOptions=
AppName=${sources.appName}
License=${path.basename(sources.licenseFilename)}
BitmapFileName=splash.gif
StartWithConfiguration=True

[Packages]
khmer_angkor.kmp
`;

    const setupInf = new TextDecoder().decode(await zipFile.file('setup.inf').async('uint8array'));
    assert.equal(setupInf.trim(), setupInfFixture.trim());

    const verifyFile = async (filename: string) => {
      const fixture = fs.readFileSync(filename);
      const file = await zipFile.file(path.basename(filename)).async('uint8array');
      assert.deepEqual(file, fixture, `File in zip '${filename}' did not match fixture`);
    };

    await verifyFile(sources.msiFilename);
    await verifyFile(sources.licenseFilename);

    // We only test for existence of the file in the zip for now
    const kmp = await zipFile.file('khmer_angkor.kmp').async('uint8array');
    assert.isNotEmpty(kmp);
  });
});
