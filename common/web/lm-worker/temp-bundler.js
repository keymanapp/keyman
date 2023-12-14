import { iifeConfiguration, forES6, prepareTslibTreeshaking } from '../es-bundling/build/index.mjs';
import esbuild from 'esbuild';
import fs from 'fs';

const localConfig = {
  ...forES6(iifeConfiguration),
  entryPoints: {
    'index': './src/main/worker-main.ts',
  },
  minify: true,
  outfile: './build/lib/temp/worker-main.js',
  // `esbuild`'s sourcemap output puts relative paths to the original sources from the
  // directory of the build output.  The following keeps repo structure intact and
  // puts our code under a common 'namespace' of sorts.
  sourceRoot: '@keymanapp/keyman/common/web/lm-worker/src/main',
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