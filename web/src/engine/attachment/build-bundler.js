/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';
import { esmConfiguration, bundleObjEntryPoints } from '../../../../common/web/es-bundling/build/index.mjs';

await esbuild.build({
  ...esmConfiguration,
  ...bundleObjEntryPoints('lib', '../../../build/engine/attachment/obj/index.js')
});
