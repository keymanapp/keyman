import esbuild from 'esbuild';

await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "esm",
  nodePaths: ['../../../node_modules'],
  entryPoints: {
    'index': '../../../build/tools/obj/index.js',
  },
  outdir: '../../../build/tools/lib/',
  outExtension: { '.js': '.mjs' },
  tsconfig: './tsconfig.json',
  target: "es5"
});
