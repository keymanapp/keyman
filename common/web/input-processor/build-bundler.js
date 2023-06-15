import esbuild from 'esbuild';
import { esmConfiguration, bundleObjEntryPointsAsLib } from '../es-bundling/build/index.mjs';

// Bundled ES module version
await esbuild.build({
  ...esmConfiguration,
  ...bundleObjEntryPointsAsLib('build/obj/index.js')
});