import esbuild from 'esbuild';
import { esmConfiguration, bundleObjEntryPoints } from '../../../../common/web/es-bundling/build/index.mjs';

await esbuild.build({
  ...esmConfiguration,
  ...bundleObjEntryPoints('lib', '../../../build/engine/osk/obj/index.js')
});