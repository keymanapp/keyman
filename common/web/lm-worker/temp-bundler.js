import { pluginForDowncompiledClassTreeshaking, prepareTslibTreeshaking } from '../es-bundling/build/index.mjs';
import esbuild from 'esbuild';
import fs from 'fs';

const esmConfiguration = {
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  minify: true,
  format: "iife",
  // minifyWhitespace: true,
  // minifySyntax: true,
  // // minifyIdentifiers: true,
  plugins: [ pluginForDowncompiledClassTreeshaking ],
  sourcemap: true,
  sourcesContent: true,
  target: "es6",
  outExtension: { '.js': '.js'},
  treeShaking: true,
  conditions: ['es6-bundling']
};

const localConfig = {
  ...esmConfiguration,
  entryPoints: {
    'index': './src/main/index.ts',
  },
  outfile: './build/lib/temp/index.js',
  // `esbuild`'s sourcemap output puts relative paths to the original sources from the
  // directory of the build output.  The following keeps repo structure intact and
  // puts our code under a common 'namespace' of sorts.
  sourceRoot: '@keymanapp/keyman/common/web/lm-worker/src/main',
  tsconfig: 'tsconfig.json',
  // temp
  metafile: true
}

await prepareTslibTreeshaking(localConfig);

let result = await esbuild.build(localConfig);

let filesizeProfile = await esbuild.analyzeMetafile(result.metafile, { verbose: true });

fs.writeFileSync('./build/lib/temp/filesize-profile.log', `
// Minified Worker filesize profile, before polyfilling
${filesizeProfile}
`);