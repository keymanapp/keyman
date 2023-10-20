/**
 * Create a .exe installer that bundles one or more .kmp files, together with
 * setup.exe, keymandesktop.msi, and generates and includes a setup.inf also.
 *
 * This module is effectively deprecated, but is present to keep parity with the
 * legacy kmcomp compiler. Thus, it is included as part of the package compiler,
 * and will be removed in a future version.
 *
 * This tool assumes that the installer .msi is the same version as the
 * compiler, unlike the legacy compiler which read this metadata from the .msi.
 */

import JSZip from 'jszip';
import { CompilerCallbacks, KeymanFileTypes, KmpJsonFile, KpsFile } from "@keymanapp/common-types";
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { KmpCompiler } from "./kmp-compiler.js";
import { CompilerMessages } from "./messages.js";

const SETUP_INF_FILENAME = 'setup.inf';
const PRODUCT_NAME = 'Keyman';

export interface WindowsPackageInstallerSources {
  msiFilename: string;
  setupExeFilename: string;
  licenseFilename: string; // MIT license
  titleImageFilename?: string;

  appName?: string;
  startDisabled: boolean;
  startWithConfiguration: boolean;
};

export class WindowsPackageInstallerCompiler {
  private kmpCompiler: KmpCompiler;

  constructor(private callbacks: CompilerCallbacks) {
    this.kmpCompiler = new KmpCompiler(this.callbacks);
  }

  public async compile(kpsFilename: string, sources: WindowsPackageInstallerSources): Promise<Uint8Array> {
    const kps = this.kmpCompiler.loadKpsFile(kpsFilename);

    // Check existence of required files
    for(const filename of [sources.licenseFilename, sources.msiFilename, sources.setupExeFilename]) {
      if(!this.callbacks.fs.existsSync(filename)) {
        this.callbacks.reportMessage(CompilerMessages.Error_FileDoesNotExist({filename}));
        return null;
      }
    }

    // Check existence of optional files
    for(const filename of [sources.titleImageFilename]) {
      if(filename && !this.callbacks.fs.existsSync(filename)) {
        this.callbacks.reportMessage(CompilerMessages.Error_FileDoesNotExist({filename}));
        return null;
      }
    }

    // Note: we never use the MSIFileName field from the .kps any more
    // Nor do we use the MSIOptions field.

    // Build the zip
    const zipBuffer = await this.buildZip(kps, kpsFilename, sources);
    if(!zipBuffer) {
      // Error messages already reported by buildZip
      return null;
    }

    // Build the sfx
    const sfxBuffer = this.buildSfx(zipBuffer, sources);
    return sfxBuffer;
  }

  private async buildZip(kps: KpsFile.KpsFile, kpsFilename: string, sources: WindowsPackageInstallerSources): Promise<Uint8Array> {
    const kmpJson: KmpJsonFile.KmpJsonFile = this.kmpCompiler.transformKpsFileToKmpObject(kpsFilename, kps);
    if(!kmpJson.info?.name?.description) {
      this.callbacks.reportMessage(CompilerMessages.Error_PackageNameCannotBeBlank());
      return null;
    }

    const kmpFilename = this.callbacks.path.basename(kpsFilename, KeymanFileTypes.Source.Package) + KeymanFileTypes.Binary.Package;
    const setupInfBuffer = this.buildSetupInf(sources, kmpJson, kmpFilename, kps);
    const kmpBuffer = await this.kmpCompiler.buildKmpFile(kpsFilename, kmpJson);

    // Note that this does not technically generate a "valid" sfx according to
    // the zip spec, because the offsets in the .zip are relative to the start
    // of the zip, rather than to the start of the sfx. However, as Keyman's
    // installer chops out the zip from the sfx and loads it in a new stream, it
    // works as expected.
    const zip = JSZip();
    zip.file(SETUP_INF_FILENAME, setupInfBuffer);
    zip.file(kmpFilename, kmpBuffer);
    zip.file(this.callbacks.path.basename(sources.msiFilename), this.callbacks.loadFile(sources.msiFilename));
    zip.file(this.callbacks.path.basename(sources.licenseFilename), this.callbacks.loadFile(sources.licenseFilename));
    if(sources.titleImageFilename) {
      zip.file(this.callbacks.path.basename(sources.titleImageFilename), this.callbacks.loadFile(sources.titleImageFilename));
    }

    return zip.generateAsync({type: 'uint8array', compression:'DEFLATE'});
  }

  private buildSetupInf(sources: WindowsPackageInstallerSources, kmpJson: KmpJsonFile.KmpJsonFile, kmpFilename: string, kps: KpsFile.KpsFile) {
    let setupInf = `[Setup]
Version=${KEYMAN_VERSION.VERSION}
MSIFileName=${this.callbacks.path.basename(sources.msiFilename)}
MSIOptions=
AppName=${sources.appName ?? PRODUCT_NAME}
License=${this.callbacks.path.basename(sources.licenseFilename)}
`;
    if (sources.titleImageFilename) {
      setupInf += `TitleImage=${this.callbacks.path.basename(sources.titleImageFilename)}\n`;
    }
    if (kmpJson.options.graphicFile) {
      setupInf += `BitmapFileName=${this.callbacks.path.basename(kmpJson.options.graphicFile)}\n`;
    }
    if (sources.startDisabled) {
      setupInf += `StartDisabled=True\n`;
    }
    if (sources.startWithConfiguration) {
      setupInf += `StartWithConfiguration=True\n`;
    }

    setupInf += `\n[Packages]\n`;
    setupInf += kmpFilename + '\n';
    // TODO: multiple packages?
    const strings = !kps.Strings?.String ? [] : (Array.isArray(kps.Strings.String) ? kps.Strings.String : [kps.Strings.String]);
    if (strings.length) {
      setupInf += `\n[Strings]\n`;
      for (const str of strings) {
        setupInf += `${str.$?.Name}=${str.$?.Value}\n`;
      }
    }

    const setupInfBuffer = new TextEncoder().encode(setupInf);
    return setupInfBuffer;
  }

  private buildSfx(zipBuffer: Uint8Array, sources: WindowsPackageInstallerSources): Uint8Array {
    const setupRedistBuffer = this.callbacks.loadFile(sources.setupExeFilename);
    const buffer = new Uint8Array(setupRedistBuffer.length + zipBuffer.length);
    buffer.set(setupRedistBuffer, 0);
    buffer.set(zipBuffer, setupRedistBuffer.length);
    return buffer;
  }
}
