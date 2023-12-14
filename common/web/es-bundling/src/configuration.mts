import type * as esbuild from 'esbuild';
import { pluginForDowncompiledClassTreeshaking } from './classTreeshaker.mjs';

export const esmConfiguration: esbuild.BuildOptions = {
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  format: "esm",
  outExtension: { '.js': '.mjs'},
  plugins: [ pluginForDowncompiledClassTreeshaking ],
  sourcemap: true,
  sourcesContent: true,
  target: "es5"
};

export const iifeConfiguration: esbuild.BuildOptions = {
  ...esmConfiguration,
  format: 'iife',
  outExtension: { '.js': '.js'},
  treeShaking: true
};

export function forES6(config: esbuild.BuildOptions): esbuild.BuildOptions {
  return {
    ...config,
    target: "es6",
    conditions: ['es6-bundling'],
    // Even if they'd be tree-shaken out, esbuild will fall over from their presence
    // within an imported `@keymanapp/common-types`.
    external: ['timers', 'buffer', 'events'],
    // Available with ES6, but not necessarily with ES5.
    keepNames: true
  };
}

export function objRoot(entryPath: string) {
  const OBJ_REGEX = /^(.+)\/obj\//;
  const match = entryPath.match(OBJ_REGEX);

  return `${match[1]}`;
}

/**
 * Given the KMW build output convention of raw TS output within a subfolder of `build` named `obj`,
 * this configures esbuild to use the specified entry points for bundling and to have their output
 * bundle locations rooted within a specified sibling folder to their "source" `obj` subfolder.
 *
 * Examples:
 * 1. 'lib', 'build/obj/index.js' => 'build/lib/index.js'
 * 2. 'debug', 'build/app/ui/obj/kmwuitoggle.js' => 'build/app/browser/debug/kmwuitoggle.js'
 *
 * @param path
 * @configFolder
 * @returns
 */
export function bundleObjEntryPoints(configFolder: 'lib' | 'debug' | 'release', ...path: string[]): esbuild.BuildOptions {
  if(!path.length) {
    throw new Error("Must specify at least one entry point.");
  }

  let mappedRoot = objRoot(path[0]);
  if(path.map((entry) => objRoot(entry)).find((mappedEntry) => mappedEntry != mappedRoot)) {
    throw new Error("The specified entry points map to different roots, therefore mapping is not 'standard'.");
  }

  mappedRoot = `${mappedRoot}/${configFolder}/`

  return {
    entryPoints: path,
    outdir: mappedRoot
  };
}