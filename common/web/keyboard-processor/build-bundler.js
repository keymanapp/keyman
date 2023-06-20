import esbuild from 'esbuild';
import { esmConfiguration, bundleObjEntryPoints } from '../es-bundling/build/index.mjs';
import * as fs from 'fs';

// Bundled ES module version
await esbuild.build({
  ...esmConfiguration,
  ...bundleObjEntryPoints('lib', 'build/obj/index.js', 'build/obj/keyboards/loaders/dom-keyboard-loader.js')
});

// Sadly, esbuild aims to preserve the relative path between entry points... we want it directly
// in /build/lib, not in a buried subfolder.  (This matters most for explicitly DOM libs like this
// one, since we can't do import path resolution in DOM tests like we can for headless tests.)
//
// Alternatively, we can just build it separately like the node-oriented one.
fs.renameSync('build/lib/keyboards/loaders/dom-keyboard-loader.mjs', 'build/lib/dom-keyboard-loader.mjs');
fs.rmSync('build/lib/keyboards', { recursive: true, force: true });

// The node-based keyboard loader needs an extra parameter due to Node-built-in imports:
await esbuild.build({
  ...esmConfiguration,
  ...bundleObjEntryPoints('lib', 'build/obj/keyboards/loaders/node-keyboard-loader.js'),
  platform: "node"
});