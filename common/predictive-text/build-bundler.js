import esbuild from 'esbuild';
import { esmConfiguration } from '../web/es-bundling/build/index.mjs';

await esbuild.build({
  ...esmConfiguration,
  entryPoints: {
    'index': 'build/obj/web/index.js',
  },
  outdir: 'build/lib/web' // We want to preserve the subdirectory for this one.
});