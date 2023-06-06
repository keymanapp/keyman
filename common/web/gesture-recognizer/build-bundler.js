import esbuild from 'esbuild';

await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "esm",
  nodePaths: ['../../../node_modules'],
  entryPoints: {
    'index': 'build/obj/index.js',
  },
  outdir: 'build/lib/',
  outExtension: { '.js': '.mjs' },
  tsconfig: './tsconfig.json',
  target: "es5"
});
