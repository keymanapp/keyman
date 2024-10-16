import esbuild from 'esbuild';
import { esmConfiguration, bundleObjEntryPoints } from '../../../../../../tools/es-bundling/build/index.mjs';

await esbuild.build({
  ...esmConfiguration,
  ...bundleObjEntryPoints('lib', '../../../build/tools/obj/index.js')
});