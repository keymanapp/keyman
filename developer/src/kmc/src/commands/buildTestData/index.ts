import * as fs from 'fs';
import * as path from 'path';
import * as kmcLdml from '@keymanapp/kmc-ldml';
import { CompilerCallbacks, defaultCompilerOptions, LDMLKeyboardTestDataXMLSourceFile, LDMLKeyboardXMLSourceFileReader, LKTAnyAction } from '@keymanapp/developer-utils';
import { NodeCompilerCallbacks } from '../../util/NodeCompilerCallbacks.js';
import { fileURLToPath } from 'url';
import { CommandLineBaseOptions } from 'src/util/baseOptions.js';
import { exitProcess } from '../../util/sysexits.js';
import { InfrastructureMessages } from '../../messages/infrastructureMessages.js';
import { dirname } from 'node:path';
import { KeymanXMLReader, LKTTest, LKTKeystroke, LKTCheck } from '@keymanapp/developer-utils';

export async function buildTestData(infile: string, _options: any, commander: any): Promise<void> {
  const options: CommandLineBaseOptions = commander.optsWithGlobals();

  let compilerOptions: kmcLdml.LdmlCompilerOptions = {
    ...defaultCompilerOptions,
    ...options,
    saveDebug: false,
    shouldAddCompilerVersion: false,
    readerOptions: {
      cldrImportsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL)),
      localImportsPaths: [dirname(infile)], // local dir
    }
  };

  const callbacks = new NodeCompilerCallbacks(options);

  let testData = await loadTestData(infile, callbacks, compilerOptions);
  if (!testData || callbacks.hasFailureMessage()) {
    await exitProcess(1);
  }

  const fileBaseName = options.outFile ?? infile;
  const outFileBase = path.basename(fileBaseName, path.extname(fileBaseName));
  const outFileDir = path.dirname(fileBaseName);
  const outFileJson = path.join(outFileDir, outFileBase + '.json');
  fs.writeFileSync(outFileJson, JSON.stringify(testData, null, '  '));
  callbacks.reportMessage(InfrastructureMessages.Info_FileBuiltSuccessfully({ filename: outFileJson, relativeFilename: infile }));
}

async function loadTestData(inputFilename: string, callbacks: CompilerCallbacks, options: kmcLdml.LdmlCompilerOptions): Promise<LDMLKeyboardTestDataXMLSourceFile> {
  const k = new kmcLdml.LdmlKeyboardCompiler();
  if (!await k.init(callbacks, options)) {
    return null;
  }
  return await k.loadTestData(inputFilename);
}


export async function buildLdmlRegressionData(regressionDirectory: string, _options: any, commander: any): Promise<void> {
  const options: CommandLineBaseOptions = commander.optsWithGlobals();

  // we need to recompile the .xml as we'll need metadata from it
  const { keyboard } = _options;

  const infile = fs.realpathSync.native(keyboard);

  const ldmlCompilerOptions: kmcLdml.LdmlCompilerOptions = {
    ...options, readerOptions: {
      cldrImportsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL)),
      localImportsPaths: [dirname(infile)], // local dir
    }
  };
  const compiler = new kmcLdml.LdmlKeyboardCompiler();
  const callbacks = new NodeCompilerCallbacks(options);
  await compiler.init(callbacks, ldmlCompilerOptions);
  let source = compiler.load(infile);
  if (!source) {
    return; // has callbacks
  }
  let kmx = await compiler.compile(source);
  if (!kmx) {
    return; // sets callbacks
  }

  const xreader = new KeymanXMLReader('regressiontest');

  // list of xmls
  const xmls = fs.readdirSync(regressionDirectory).filter(f => /\.xml$/.test(f));
  if (xmls.length == 0) {
    throw Error(`Directory ${regressionDirectory} had zero .xml files`);
  }

  const testFile: LDMLKeyboardTestDataXMLSourceFile = {
    keyboardTest3: {
      conformsTo: '45',
      info: {
        author: kmx.kmxplus.meta?.author.value,
        keyboard: path.basename(keyboard),
        name: kmx.kmxplus.meta?.name?.value,
      },
      repertoire: [],
      tests: [],
    },
  };

  const coll = new Intl.Collator(['en'], {
    numeric: true
  });
  for (const testName of xmls.sort((a, b) => coll.compare(a, b))) {
    const name = path.basename(testName, '.xml');
    const full = path.join(regressionDirectory, testName);
    // XML read
    const xdata = xreader.parse(fs.readFileSync(full, 'utf-8'));
    console.dir({xdata}, { depth: Infinity });
    let events = xdata.regressiontest.events.event;
    // boxing
    if (!Array.isArray(events)) {
      events = [events];
    }
    const actions : LKTAnyAction[] = [];
    for (const event of events) {
      actions.push(<LKTKeystroke>{
        type: "keystroke",
        keystroke: {
          key: event.key.vkey,
        }
      });
      actions.push(<LKTCheck>{
        type: "check",
        check: {
          result: event.postcontext.text,
        }
      });
    }

    const tests: LKTTest[] = [
      {
        name: 'converted',
        startContext: {
          to: '',
        },
        actions,
      }
    ];

    // TODO: add tests

    testFile.keyboardTest3.tests.push({
      name,
      test: tests,
    });
  }

  console.dir(testFile, { depth: Infinity });

  // console.dir({kmx});

  // return await super.runCompiler(compiler, infile, outfile, callbacks, ldmlCompilerOptions);

  // // options for the keyboards
  // let testCompilerOptions: kmcLdml.LdmlCompilerOptions = {
  //   ...defaultCompilerOptions,
  //   ...options,
  //   saveDebug: false,
  //   shouldAddCompilerVersion: false,
  //   readerOptions: {
  //     cldrImportsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL)),
  //     localImportsPaths: [ dirname(infile) ], // local dir
  //   }
  // };


  // let testData = await loadTestData(infile, callbacks, compilerOptions);
  // if (!testData || callbacks.hasFailureMessage()) {
  //   await exitProcess(1);
  // }

  const fileBaseName = options.outFile ?? infile;
  const outFileBase = path.basename(fileBaseName, path.extname(fileBaseName));
  const outFileDir = path.dirname(fileBaseName);
  const outFileJson = path.join(outFileDir, outFileBase + '.json');
  fs.writeFileSync(outFileJson, JSON.stringify(testFile, null, '  '));
  callbacks.reportMessage(InfrastructureMessages.Info_FileBuiltSuccessfully({filename: outFileJson, relativeFilename:  infile}));
}
