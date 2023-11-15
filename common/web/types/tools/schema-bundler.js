/*
 * Bundle schema validation files (from .cjs) and make them available as ES modules
 */

import esbuild from 'esbuild';

await esbuild.build({
  entryPoints: [
    'obj/schemas/kpj.schema.validator.cjs',
    'obj/schemas/kpj-9.0.schema.validator.cjs',
    'obj/schemas/kvks.schema.validator.cjs',
    'obj/schemas/ldml-keyboard3.schema.validator.cjs',
    'obj/schemas/ldml-keyboardtest3.schema.validator.cjs',
    'obj/schemas/displaymap.schema.validator.cjs',
    'obj/schemas/keyman-touch-layout.clean.spec.validator.cjs',
    'obj/schemas/keyman-touch-layout.spec.validator.cjs',
    'obj/schemas/keyboard_info.schema.validator.cjs',
    'obj/schemas/kmp.schema.validator.cjs',
  ],
  bundle: true,
  format: 'esm',
  target: 'es2022',
  outdir: 'src/schemas/',
  sourcemap: false,

  // We want a .mjs extension to force node into ESM module mode
  outExtension: { '.js': '.mjs' },
});
