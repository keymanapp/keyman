import { iifeConfiguration, forES6, prepareTslibTreeshaking } from '../../../../common/web/es-bundling/build/index.mjs';
import esbuild from 'esbuild';
import fs from 'fs';

const localConfig = {
  ...forES6(iifeConfiguration),
  entryPoints: {
    'index': './src/release-main.ts',
  },
  minify: true,
  outfile: '../../../build/app/browser/temp/keymanweb.js',
  // `esbuild`'s sourcemap output puts relative paths to the original sources from the
  // directory of the build output.  The following keeps repo structure intact and
  // puts our code under a common 'namespace' of sorts.
  sourceRoot: '@keymanapp/keyman/web/src/app/browser/src',
  // temp
  metafile: true
}

await prepareTslibTreeshaking(localConfig);

let result = await esbuild.build(localConfig);

let filesizeProfile = await esbuild.analyzeMetafile(result.metafile, { verbose: true });

fs.writeFileSync('../../../build/app/browser/temp/filesize-profile.log', `
// Minified Keyman Engine for Web ('app/browser' target), filesize profile
${filesizeProfile}
`);