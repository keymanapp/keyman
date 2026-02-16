import esbuild from 'esbuild';
import * as fs from 'fs';

// Note:  this package is intended 100% as a dev-tool, hence why esbuild is just a dev-dependency.

// Component #1:  Detect all `tslib` helpers we actually want to use.
export async function determineNeededDowncompileHelpers(config: esbuild.BuildOptions, ignoreFilePattern?: RegExp, log?: boolean) {
  const tslibHelperNames = [
    "__extends",
    "__assign",
    "__rest",
    "__decorate",
    "__param",
    "__metadata",
    "__awaiter",
    "__generator",
    "__exportStar",  // uses __createBinding
    "__createBinding",
    "__values",
    "__read",
    "__spread",
    "__spreadArray",
    "__spreadArrays",
    "__await",
    "__asyncGenerator",
    "__asyncDelegator",
    "__asyncValues",
    "__makeTemplateObject",
    "__importStar",
    "__setModuleDefault", // only used by the previous entry!
    "__importDefault",
    "__classPrivateFieldGet",
    "__classPrivateFieldSet",
    "__classPrivateFieldIn",
    "__runInitializers",
    "__setFunctionName",
    "__propKey",
    "__esDecorate"
  ];

  const detectedHelpers: string[] = [];

  const pluginName = 'es-bundling:  tslib helper use detection'
  let tslibHelperDetectionPlugin = {
    name: pluginName,
    setup(build: esbuild.PluginBuild) {
      build.onLoad({filter: /\.js$/}, async (args) => {
        let source = await fs.promises.readFile(args.path, 'utf8');

        let warnings: esbuild.Message[] = [];

        if(/tslib.js$/.test(args.path)) {
          let declarationRegex = /var (__[a-zA-Z0-9]+)/g;
          let results = source.match(declarationRegex);

          if(!results) {
            return buildMessage(
              pluginName,
              "tslib's definition pattern has shifted dramatically!  tslib helper-definition detector maintenance is needed.",
              indexToSourceLocation('', 0, 0, args.path)
            );
          }

          for(let result of results) {
            let capture = result.substring(4);

            if(!tslibHelperNames.find((entry) => entry == capture)) {
              // Match:  `result` itself.  Grab index, build location, build message.
              let index = source.indexOf(result);

              warnings.push(buildMessage(
                pluginName,
                'Probable `tslib` helper `' + capture + '` has not been validated for tree-shaking potential',
                indexToSourceLocation(source, index, result.length, args.path)
              ));
            }
          }

          // Returning `undefined` for `content` makes this 'pass-through' - it doesn't prevent other
          // configured plugins from working.
          return {
            warnings: warnings
          };
        }

        if(ignoreFilePattern?.test(args.path)) {
          return {};
        }

        for(let helper of tslibHelperNames) {
          if(source.indexOf(helper) > -1 && !detectedHelpers.find((entry) => entry == helper)) {
            detectedHelpers.push(helper);

            if(helper == '__importStar') {
              detectedHelpers.push('__setModuleDefault');
              detectedHelpers.push('__createBinding');
            } else if(helper == '__exportStar') {
              detectedHelpers.push('__createBinding');
            } else if(helper == '__spread') {
              detectedHelpers.push('__read');
            }
          }
        }

        // Returning an empty object makes this 'pass-through' - it doesn't prevent other
        // configured plugins from working.
        return {};
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

function indexToSourceLocation(source: string, index: number, matchLength: number, file: string): esbuild.Location {
  let priorText = source.substring(0, index);
  let lineNum = priorText.split('\n').length;
  let lastLineBreakIndex = priorText.lastIndexOf('\n');
  let colNum = index - lastLineBreakIndex;
  let nextLineBreakIndex = source.indexOf('\n', lastLineBreakIndex+1);

  return {
    file: file,
    line: lineNum,
    column: colNum,
    lineText: source.substring(lastLineBreakIndex+1, nextLineBreakIndex),
    namespace: '',
    suggestion: '',
    length: matchLength
  }
}

function buildMessage(pluginName: string, message: string, location: esbuild.Location): esbuild.Message {
  return {
    id: '',
    notes: [],
    detail: '',
    pluginName: pluginName,
    text: message,
    location: location
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

  treeshakeDefinition(source: string, start: number, expectTernary: boolean, name: string, location: esbuild.Location) {
    let scopeDepth = 0;
    let parenDepth = 0;
    let topLevelTernaryActive = 0;
    let i = start;
    let char = source.charAt(i);
    while(char != '}' || --scopeDepth != 0 || topLevelTernaryActive) {
      if(char == '{') {
        scopeDepth++;
      } else if(char == '(') {
        parenDepth++;
      } else if(char == ')') {
        parenDepth--;
      } else if(char == '?' && scopeDepth == 0) {
        if(!expectTernary) {
          return {
            source: source,
            error: buildMessage(this.name, `Unexpected conditional for definition of ${name}`, location)
          }
        }
        topLevelTernaryActive++;
      } else if(char == ':' && topLevelTernaryActive && scopeDepth == 0) {
        topLevelTernaryActive--;
      }
      i++;

      if(i > source.length) {
        return {
          source: source,
          error: buildMessage(this.name, `Failed to determine end of definition for ${name}`, location)
        };
      }

      char = source.charAt(i);
    }
    i++; // we want to erase the final '}', too.

    // The functions may be wrapped with parens.
    if(parenDepth == 1) {
      let nextOpen = source.indexOf('(', i);
      let nextClosed = source.indexOf(')', i);

      if(nextOpen < nextClosed) {
        return {
          source: source,
          error: buildMessage(this.name, `Failed to determine end of definition for ${name}`, location)
        };
      } else {
        i = nextClosed + 1;
      }
    }

    if(source.charAt(i) == ';') {
      i++;
    }

    return {
      source:  source.replace(source.substring(start, i), '')
    };
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
        let matchString = `${unusedHelper} = function`;
        let expectTernary = false;

        // A special case - it has two different versions depending on if Object.create exists or not.
        // This is established via a ternary conditional.  Just adds a bit to the parsing.
        if(unusedHelper == '__createBinding') {
          matchString = `${unusedHelper} = `;
          expectTernary = true;
        } else if(unusedHelper == '__setModuleDefault') {
          matchString = `var ${unusedHelper} = `; // is inlined, not declared at top!
          expectTernary = true;
        } else if(unusedHelper == '__assign') {
          matchString = `${unusedHelper} = Object.assign || function`;
        }
        let definitionStart = source.indexOf(matchString);

        if(definitionStart > -1) {
          const result = this.treeshakeDefinition(
            source,
            definitionStart,
            expectTernary,
            unusedHelper,
            indexToSourceLocation(trueSource, trueSource.indexOf(matchString), matchString.length, args.path)
          );

          if(result.error) {
            errors.push(result.error);
          } else {
            source = result.source;
          }
          continue;
        }

        // If we reached this point, we couldn't treeshake the unused helper appropriately.
        if(definitionStart == -1) {
          // Matches the standard definition pattern's left-hand assignment component.
          matchString = `${unusedHelper} =`
          let bestGuessIndex = trueSource.indexOf(matchString);

          if(bestGuessIndex == -1) {
            // Failing the above, we match the declaration higher up within the file.
            matchString = `var ${unusedHelper}`
            bestGuessIndex = trueSource.indexOf(matchString);
          }

          // Failing THAT, we just give up and go start-of-file.
          if(bestGuessIndex == -1) {
            matchString = '';
            bestGuessIndex = 0;
          }

          warnings.push(
            buildMessage(
              this.name,
              "tslib has likely been updated recently; could not force-treeshake tslib helper " + unusedHelper,
              indexToSourceLocation(trueSource, bestGuessIndex, matchString.length, args.path)
            )
          );
        }
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

/**
 * Runs a non-output build of the build configuration to prepare the ability to treeshake unused
 * `tslib` downcompile helper functions and adds a fully-configured plugin to the configuration
 * once completed.
 * @param esbuildConfig
 * @returns The same build-configuration instance passed in
 */
export async function prepareTslibTreeshaking(esbuildConfig: esbuild.BuildOptions, ignoreFilePattern?: RegExp, log?: boolean) {
  // Prepare the needed setup for `tslib` treeshaking.
  const unusedHelpers = await determineNeededDowncompileHelpers(esbuildConfig, ignoreFilePattern, log);
  esbuildConfig.plugins = [buildTslibTreeshaker(unusedHelpers), ...esbuildConfig.plugins];

  return esbuildConfig;
}