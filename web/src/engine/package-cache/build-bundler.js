import esbuild from 'esbuild';
import { esmConfiguration, bundleObjEntryPoints } from '../../../../common/web/es-bundling/build/index.mjs';

await esbuild.build({
  ...esmConfiguration,
  ...bundleObjEntryPoints('lib', '../../../build/engine/package-cache/obj/index.js')
});

await esbuild.build({
  ...esmConfiguration,
  entryPoints: {
    'dom-cloud-requester': '../../../build/engine/package-cache/obj/domCloudRequester.js',
  },
  outdir: '../../../build/engine/package-cache/lib/',
});

await esbuild.build({
  ...esmConfiguration,
  entryPoints: {
    'node-cloud-requester': '../../../build/engine/package-cache/obj/nodeCloudRequester.js',
  },
  outdir: '../../../build/engine/package-cache/lib/',
  platform: 'node'
});