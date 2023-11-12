import esbuild from 'esbuild';
import { esmConfiguration, bundleObjEntryPoints, prepareTslibTreeshaking } from '../es-bundling/build/index.mjs';

await esbuild.build(await prepareTslibTreeshaking({
  ...esmConfiguration,
  ...bundleObjEntryPoints('lib', 'build/obj/index.js')
}));
