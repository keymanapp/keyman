/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
import { iifeConfiguration, prepareTslibTreeshaking  } from '../../../../common/web/es-bundling/build/index.mjs';

const commonConfig = {
  ...iifeConfiguration,
  entryPoints: {
    'index': '../../../build/app/webview/obj/debug-main.js',
  },
  outfile: '../../../build/app/webview/debug/keymanweb-webview.js',
  // `esbuild`'s sourcemap output puts relative paths to the original sources from the
  // directory of the build output.  The following keeps repo structure intact and
  // puts our code under a common 'namespace' of sorts.
  sourceRoot: '@keymanapp/keyman/web/build/app/webview/debug/'
};

await prepareTslibTreeshaking(commonConfig, /worker-main\.wrapped(?:\.min)?\.js/);

await esbuild.build(commonConfig);

await esbuild.build({
  ...commonConfig,
  entryPoints: {
    'index': '../../../build/app/webview/obj/release-main.js',
  },
  minify: true,
  outfile: '../../../build/app/webview/release/keymanweb-webview.js',
  // `esbuild`'s sourcemap output puts relative paths to the original sources from the
  // directory of the build output.  The following keeps repo structure intact and
  // puts our code under a common 'namespace' of sorts.
  sourceRoot: '@keymanapp/keyman/web/build/app/webview/debug/'
});