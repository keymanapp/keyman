/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

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
import { KeymanFileTypes, KmpJsonFile } from "@keymanapp/common-types";
import { CompilerCallbacks, KeymanCompiler, KeymanCompilerArtifact, KeymanCompilerArtifacts, KeymanCompilerResult, KpsFile, KpsFileReader } from '@keymanapp/developer-utils';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { KmpCompiler, KmpCompilerOptions } from "./kmp-compiler.js";
import { PackageCompilerMessages } from "./package-compiler-messages.js";

const SETUP_INF_FILENAME = 'setup.inf';
const PRODUCT_NAME = 'Keyman';

/**
 * @public
 * Sources and metadata for the Windows package installer compiler
 */
export interface WindowsPackageInstallerSources {
  msiFilename: string;
  setupExeFilename: string;
  licenseFilename: string; // MIT license
  titleImageFilename?: string;

  appName?: string;
  startDisabled: boolean;
  startWithConfiguration: boolean;
};

/**
 * @public
 * Options for the .kps Windows package installer compiler
 */
export interface WindowsPackageInstallerCompilerOptions extends KmpCompilerOptions {
  /**
   * Sources and metadata for the Windows package installer compiler
   */
  sources: WindowsPackageInstallerSources;
}

/**
 * @public
 * Internal in-memory build artifacts from a successful compilation
 */
export interface WindowsPackageInstallerCompilerArtifacts extends KeymanCompilerArtifacts {
  /**
   * Binary package installer filedata and filename - installable into Keyman
   * desktop and mobile projects
   */
  exe: KeymanCompilerArtifact;
};

/**
 * @public
 * Build artifacts from the .kps Windows package installer compiler
 */
export interface WindowsPackageInstallerCompilerResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link WindowsPackageInstallerCompiler.write}
   */
  artifacts: WindowsPackageInstallerCompilerArtifacts;
};

/**
 * @public
 * Compiles a .kps file to a .exe installer. The compiler does not read or write
 * from filesystem or network directly, but relies on callbacks for all external
 * IO.
 */
export class WindowsPackageInstallerCompiler implements KeymanCompiler {
  private kmpCompiler: KmpCompiler;
  private callbacks: CompilerCallbacks;
  private options: WindowsPackageInstallerCompilerOptions;

  /**
   * Initialize the compiler.
   * Copies options.
   * @param callbacks - Callbacks for external interfaces, including message
   *                    reporting and file io
   * @param options   - Compiler options
   * @returns false if initialization fails
   */
  async init(callbacks: CompilerCallbacks, options: WindowsPackageInstallerCompilerOptions): Promise<boolean> {
    this.callbacks = callbacks;
    this.options = {...options};
    this.kmpCompiler = new KmpCompiler();
    return await this.kmpCompiler.init(callbacks, options);
  }

  /**
   * Compiles a .kps file to .exe Windows package installer file. Returns an
   * object containing binary artifacts on success. The files are passed in by
   * name, and the compiler will use callbacks as passed to the
   * {@link WindowsPackageInstallerCompiler.init} function to read any input
   * files by disk.
   * @param infile  - Path to source file. Path will be parsed to find relative
   *                  references in the .kmn file, such as icon or On Screen
   *                  Keyboard file
   * @param outfile - Path to output file. The file will not be written to, but
   *                  will be included in the result for use by
   *                  {@link WindowsPackageInstallerCompiler.write}.
   * @returns         Binary artifacts on success, null on failure.
   */
  public async run(inputFilename: string, outputFilename?: string): Promise<WindowsPackageInstallerCompilerResult> {
    const sources = this.options.sources;
    const reader = new KpsFileReader(this.callbacks);
    const data = this.callbacks.loadFile(inputFilename);
    if(!data) {
      this.callbacks.reportMessage(PackageCompilerMessages.Error_FileDoesNotExist({filename: inputFilename}));
      return null;
    }

    const kps = reader.read(data);
    if(!kps) {
      // errors will already have been reported by KpsFileReader
      return null;
    }

    // Check existence of required files
    for(const [param,filename] of [
      ['licenseFilename',sources.licenseFilename],
      ['msiFilename',sources.msiFilename],
      ['setupExeFilename',sources.setupExeFilename]
    ]) {
      if(!filename) {
        this.callbacks.reportMessage(PackageCompilerMessages.Error_RequiredParameterMissing({param}));
        return null;
      }
      if(!this.callbacks.fs.existsSync(filename)) {
        this.callbacks.reportMessage(PackageCompilerMessages.Error_FileDoesNotExist({filename}));
        return null;
      }
    }

    // Check existence of optional files
    for(const filename of [sources.titleImageFilename]) {
      if(filename && !this.callbacks.fs.existsSync(filename)) {
        this.callbacks.reportMessage(PackageCompilerMessages.Error_FileDoesNotExist({filename}));
        return null;
      }
    }

    // Note: we never use the MSIFileName field from the .kps any more
    // Nor do we use the MSIOptions field.

    // Build the zip
    const zipBuffer = await this.buildZip(kps.Package, inputFilename, sources);
    if(!zipBuffer) {
      // Error messages already reported by buildZip
      return null;
    }

    // Build the sfx
    const sfxBuffer = this.buildSfx(zipBuffer, sources);

    const result: WindowsPackageInstallerCompilerResult = {
      artifacts: {
        exe: {
          data: sfxBuffer,
          filename: outputFilename ?? inputFilename.replace(/\.kps$/, '.exe')
        }
      }
    };

    return result;
  }

  /**
   * Write artifacts from a successful compile to disk, via callbacks methods.
   * The artifacts written may include:
   *
   * - .exe file - binary Windows package installer executable file
   *
   * @param artifacts - object containing artifact binary data to write out
   * @returns true on success
   */
  public async write(artifacts: WindowsPackageInstallerCompilerArtifacts): Promise<boolean> {
    this.callbacks.fs.writeFileSync(artifacts.exe.filename, artifacts.exe.data);
    return true;
  }

  private async buildZip(kps: KpsFile.KpsFile, kpsFilename: string, sources: WindowsPackageInstallerSources): Promise<Uint8Array> {
    const { kmpJsonData, fileData } = await this.kmpCompiler.transformKpsFileToKmpObject(kpsFilename, kps) ?? {};
    if(!kmpJsonData?.info?.name?.description) {
      this.callbacks.reportMessage(PackageCompilerMessages.Error_PackageNameCannotBeBlank());
      return null;
    }

    const kmpFilename = this.callbacks.path.basename(kpsFilename, KeymanFileTypes.Source.Package) + KeymanFileTypes.Binary.Package;
    const setupInfBuffer = this.buildSetupInf(sources, kmpJsonData, kmpFilename, kps);
    const kmpBuffer = await this.kmpCompiler.buildKmpFile(kmpJsonData, fileData);

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
