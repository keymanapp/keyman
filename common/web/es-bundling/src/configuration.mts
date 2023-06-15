import type * as esbuild from 'esbuild';
import { pluginForDowncompiledClassTreeshaking } from './classTreeshaker.mjs';

export const esmConfiguration: esbuild.BuildOptions = {
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  outExtension: { '.js': '.mjs'},
  plugins: [ pluginForDowncompiledClassTreeshaking ],
  sourcemap: true,
  sourcesContent: true,
  format: "esm",
  target: "es5"
};

export function objToLibRoot(entryPath: string) {
  const OBJ_REGEX = /^(.+)\/obj\//;
  const match = entryPath.match(OBJ_REGEX);

  return `${match[1]}/lib/`;
}

/**
 * Given the KMW build output convention of raw TS output in build/obj or a subdirectory thereof,
 * with bundled output in build/lib directly, this produces the standard input-to-bundled-output
 * mapping for `esbuild`'s configuration settings.
 *
 * @param path
 * @returns
 */
export function bundleObjEntryPointsAsLib(...path: string[]): esbuild.BuildOptions {
  if(!path.length) {
    throw new Error("Must specify at least one entry point.");
  }

  let mappedRoot = objToLibRoot(path[0]);
  if(path.map((entry) => objToLibRoot(entry)).find((mappedEntry) => mappedEntry != mappedRoot)) {
    throw new Error("The specified entry points map to different roots, therefore mapping is not 'standard'.");
  }

  return {
    entryPoints: path,
    outdir: mappedRoot
  };
}