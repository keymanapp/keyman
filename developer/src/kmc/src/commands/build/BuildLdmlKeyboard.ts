import * as path from 'path';
import * as fs from 'fs';
import * as kmcLdml from '@keymanapp/kmc-ldml';
import { KvkFileWriter, CompilerCallbacks } from '@keymanapp/common-types';
import { BuildActivity, BuildActivityOptions } from './BuildActivity.js';

export class BuildLdmlKeyboard extends BuildActivity {
  public get name(): string { return 'LDML keyboard'; }
  public get sourceExtension(): string { return '.xml'; }
  public get compiledExtension(): string { return '.kmx'; }
  public get description(): string { return 'Build a LDML keyboard'; }
  public async build(infile: string, callbacks: CompilerCallbacks, options: BuildActivityOptions): Promise<boolean> {
    // TODO-LDML: consider hardware vs touch -- touch-only layout will not have a .kvk
    // Compile:
    let [kmx,kvk,kmw] = buildLdmlKeyboardToMemory(infile, callbacks, options);
    // Output:

    const fileBaseName = options.outFile ?? infile;
    const outFileBase = path.basename(fileBaseName, path.extname(fileBaseName));
    const outFileDir = path.dirname(fileBaseName);

    if(kmx && kvk) {
      const outFileKmx = path.join(outFileDir, outFileBase + '.kmx');
      console.log(`Writing compiled keyboard to ${outFileKmx}`);
      fs.writeFileSync(outFileKmx, kmx);

      const outFileKvk = path.join(outFileDir, outFileBase + '.kvk');
      console.log(`Writing compiled visual keyboard to ${outFileKvk}`);
      fs.writeFileSync(outFileKvk, kvk);
    } else {
      console.error(`An error occurred compiling ${infile}`);
      return false;
    }

    if(kmw) {
      const outFileKmw = path.join(outFileDir, outFileBase + '.js');
      console.log(`Writing compiled js keyboard to ${outFileKmw}`);
      fs.writeFileSync(outFileKmw, kmw);
    }

    return true;
  }
}

function buildLdmlKeyboardToMemory(inputFilename: string, callbacks: CompilerCallbacks, options: BuildActivityOptions): [Uint8Array, Uint8Array, Uint8Array] {
  let compilerOptions: kmcLdml.CompilerOptions = {
    debug: options.debug ?? false,
    addCompilerVersion: options.compilerVersion ?? true,
    // TODO: warnDeprecatedCode: options.warnDeprecatedCode,
    // TODO: treatWarningsAsErrors: options.treatWarningsAsErrors,
  }

  const k = new kmcLdml.LdmlKeyboardCompiler(callbacks, options);
  let source = k.load(inputFilename);
  if (!source) {
    return [null, null, null];
  }
  let kmx = k.compile(source);
  if (!kmx) {
    return [null, null, null];
  }

  // In order for the KMX file to be loaded by non-KMXPlus components, it is helpful
  // to duplicate some of the metadata
  kmcLdml.KMXPlusMetadataCompiler.addKmxMetadata(kmx.kmxplus, kmx.keyboard, compilerOptions);

  // Use the builder to generate the binary output file
  const builder = new kmcLdml.KMXBuilder(kmx, options.debug);
  const kmx_binary = builder.compile();

  const vkcompiler = new kmcLdml.LdmlKeyboardVisualKeyboardCompiler();
  const vk = vkcompiler.compile(source);
  const writer = new KvkFileWriter();
  const kvk_binary = writer.write(vk);

  // Note: we could have a step of generating source files here
  // KvksFileWriter()...
  // const tlcompiler = new kmc.TouchLayoutCompiler();
  // const tl = tlcompiler.compile(source);
  // const tlwriter = new TouchLayoutFileWriter();
  const kmwcompiler = new kmcLdml.LdmlKeyboardKeymanWebCompiler(callbacks, compilerOptions);
  const kmw_string = kmwcompiler.compile(inputFilename, source);
  const encoder = new TextEncoder();
  const kmw_binary = encoder.encode(kmw_string);

  return [kmx_binary, kvk_binary, kmw_binary];
}
