import * as fs from 'fs';
import * as path from 'path';
import { CompilerBaseOptions, CompilerCallbacks, defaultCompilerOptions } from '@keymanapp/common-types';
import { NodeCompilerCallbacks } from '../../util/NodeCompilerCallbacks.js';
import { WindowsPackageInstallerCompiler, WindowsPackageInstallerSources } from '@keymanapp/kmc-package';

interface WindowsPackageInstallerOptions extends CompilerBaseOptions {
  msi: string;
  exe: string;
  license: string;
  titleImage?: string;
  appName?: string;
  startDisabled: boolean;
  startWithConfiguration: boolean;
};

export async function buildWindowsPackageInstaller(infile: string, _options: any, commander: any) {
  const options: WindowsPackageInstallerOptions = commander.optsWithGlobals();
  const sources: WindowsPackageInstallerSources = {
    licenseFilename: options.license,
    msiFilename: options.msi,
    setupExeFilename: options.exe,
    startDisabled: options.startDisabled,
    startWithConfiguration: options.startWithConfiguration,
    appName: options.appName,
    titleImageFilename: options.titleImage
  }

  // Normalize case for the filename and expand the path; this avoids false
  // positive case mismatches on input filenames and glommed paths
  infile = fs.realpathSync.native(infile);

  const callbacks: CompilerCallbacks = new NodeCompilerCallbacks({...defaultCompilerOptions, ...options});
  const compiler = new WindowsPackageInstallerCompiler(callbacks);

  const buffer = await compiler.compile(infile, sources);
  if(!buffer) {
    // errors will have been reported already
    process.exit(1);
  }

  const fileBaseName = options.outFile ?? infile;
  const outFileBase = path.basename(fileBaseName, path.extname(fileBaseName));
  const outFileDir = path.dirname(fileBaseName);
  const outFileExe = path.join(outFileDir, outFileBase + '.exe');
  fs.writeFileSync(outFileExe, buffer);
}
