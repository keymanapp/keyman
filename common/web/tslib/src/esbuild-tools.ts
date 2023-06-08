import esbuild from 'esbuild';
import fs from 'fs';

// Note:  this package is intended 100% as a dev-tool, hence why esbuild is just a dev-dependency.

// Component #1:  Detect all `tslib` helpers we actually want to use.
export async function determineNeededDowncompileHelpers(config: esbuild.BuildOptions, log: boolean) {
  const tslibHelperNames = [
    "__extends",
    "__assign",
    "__rest",
    "__decorate",
    "__param",
    "__metadata",
    "__awaiter",
    "__generator",
    "__exportStar",
    "__createBinding",
    "__values",
    "__read",
    "__spread",
    "__spreadArrays",
    "__await",
    "__asyncGenerator",
    "__asyncDelegator",
    "__asyncValues",
    "__makeTemplateObject",
    "__importStar",
    "__importDefault",
    "__classPrivateFieldGet",
    "__classPrivateFieldSet"
  ];

  const detectedHelpers: string[] = [];

  let tslibHelperDetectionPlugin = {
    name: 'tslib helper use detection',
    setup(build) {
      build.onLoad({filter: /\.js$/}, async (args) => {
        //
        if(/tslib.js$/.test(args.path)) {
          // Returning `undefined` makes this 'pass-through' - it doesn't prevent other
          // configured plugins from working.
          return;
        }

        let source = await fs.promises.readFile(args.path, 'utf8');

        for(let helper of tslibHelperNames) {
          if(source.indexOf(helper) > -1 && !detectedHelpers.find((entry) => entry == helper)) {
            detectedHelpers.push(helper);
          }
        }

        // Returning `undefined` makes this 'pass-through' - it doesn't prevent other
        // configured plugins from working.
        return;
      });
    }
  }

  // tslib tree-shake phase 1 - detecting which helpers are safe to remove.
  await esbuild.build({
    ...config,
    // `tslibHelperDetectionPlugin` is pass-through, and so has no net effect on
    // the build.  We just use this run to scan for any utilized `tslib` helper funcs,
    // not to manipulate the actual source in any way.
    plugins: [ tslibHelperDetectionPlugin, ...(config.plugins ?? []) ],
    write: false
  });

  // At this point, we can determine what's unused.
  detectedHelpers.sort();

  const unusedHelpers: string[] = [];
  tslibHelperNames.forEach((entry) => {
    if(!detectedHelpers.find((detected) => detected == entry)) {
      unusedHelpers.push(entry);
    }
  });

  if(log) {
    // Logs on the tree-shaking decisions
    console.log("Detected helpers from tslib: ");
    console.log();

    console.log();
    console.log("Unused helpers: ");
    console.log(unusedHelpers);
  }

  return unusedHelpers;
}

function indexToSourcePosition(source: string, index: number) {
  let priorText = source.substring(0, index);
  let lineNum = priorText.split('\n').length;
  let lastLineBreakIndex = priorText.lastIndexOf('\n');
  let colNum = index - lastLineBreakIndex;
  let nextLineBreakIndex = source.indexOf('\n', lastLineBreakIndex+1);

  return {
    line: lineNum,
    column: colNum,
    lineText: source.substring(lastLineBreakIndex+1, nextLineBreakIndex)
  }
}

// Ugh.  https://github.com/evanw/esbuild/issues/2656#issuecomment-1304013941
function wrapClassPlugin(instancePlugin: esbuild.Plugin) {
  return {
    name: instancePlugin.name,
    setup: instancePlugin.setup.bind(instancePlugin)
  };
}

// Component #2:  when we've actually determined which ones are safe to remove, this plugin
// can remove their code.
class TslibTreeshaker implements esbuild.Plugin {
  public readonly name = 'tslib forced treeshaking';

  private unusedHelpers: string[];

  constructor(unusedHelpers: string[]) {
    this.unusedHelpers = unusedHelpers;
  }

  setup(build: esbuild.PluginBuild) {
    build.onLoad({filter: /tslib.js$/}, async (args) => {
      const trueSource = await fs.promises.readFile(args.path, 'utf8');
      let source = trueSource;

      let warnings: esbuild.Message[] = [];
      let errors: esbuild.Message[] = [];

      for(let unusedHelper of this.unusedHelpers) {
        // Removes the 'exporter' line used to actually export it from the tslib source.
        source = source.replace(`exporter\(\"${unusedHelper}\", ${unusedHelper}\);`, '');

        // Removes the actual helper function definition - obviously, the biggest filesize savings to be had here.
        let definitionStart = source.indexOf(`${unusedHelper} = function`);

        // Emission of warnings & errors
        if(definitionStart == -1) {
          let matchString = `${unusedHelper} =`
          let bestGuessIndex = trueSource.indexOf(matchString);
          if(bestGuessIndex == -1) {
            matchString = `var ${unusedHelper}`
            bestGuessIndex = trueSource.indexOf(matchString);
          }

          if(bestGuessIndex == -1) {
            matchString = '';
          }

          let logLocation = indexToSourcePosition(trueSource, bestGuessIndex);

          let location: esbuild.Location = {
            file: args.path,
            line: logLocation.line,
            column: logLocation.column,
            length: matchString.length,
            lineText: logLocation.lineText,
            namespace: '',
            suggestion: ''
          }

          if(unusedHelper == '__createBinding') {
            warnings.push({
              id: '',
              notes: [],
              detail: '',
              pluginName: this.name,
              text: "Currently unable to force-treeshake the __createBinding tslib helper",
              location: location
            });
          } else {
            warnings.push({
              id: '',
              notes: [],
              detail: '',
              pluginName: this.name,
              text: "tslib has likely been updated recently; could not force-treeshake tslib helper " + unusedHelper,
              location: location
            });
          }
          continue;
        }

        let scopeDepth = 0;
        let i = definitionStart;
        let char = source.charAt(i);
        while(char != '}' || --scopeDepth != 0) {
          if(char == '{') {
            scopeDepth++;
          }
          i++;
          char = source.charAt(i);
        }
        i++; // we want to erase it, too.

        source = source.replace(source.substring(definitionStart, i), '');

        // The top-level var declaration is auto-removed by esbuild when no references to it remain.
      }

      return {
        contents: source,
        loader: 'js',
        warnings: warnings,
        errors: errors
      };
    });
  }
};

export function buildTslibTreeshaker(unusedHelpers: string[]) {
  return wrapClassPlugin(new TslibTreeshaker(unusedHelpers));
};