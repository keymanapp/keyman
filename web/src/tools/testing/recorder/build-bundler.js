import esbuild from 'esbuild';

await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "esm",
  nodePaths: ['../../../../../node_modules'],
  entryPoints: {
    'index': '../../../../build/tools/testing/recorder/obj/index.js',
  },
  outdir: '../../../../build/tools/testing/recorder/lib/',
  outExtension: { '.js': '.mjs' },
  tsconfig: './tsconfig.json',
  target: "es5"
});