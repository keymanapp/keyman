import esbuild from 'esbuild';

await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "esm",
  nodePaths: ['../../../../node_modules'],
  entryPoints: {
    'index': '../../../build/engine/osk/obj/index.js',
  },
  external: ['fs', 'vm'],
  outdir: '../../../build/engine/osk/lib/',
  outExtension: { '.js': '.mjs' },
  tsconfig: './tsconfig.json',
  target: "es5"
});
