import esbuild from 'esbuild';
import { esmConfiguration, bundleObjEntryPoints } from '../es-bundling/build/index.mjs';

// Bundled ES module version
await esbuild.build({
  ...esmConfiguration,
  ...bundleObjEntryPoints('lib', 'build/obj/index.js')
});