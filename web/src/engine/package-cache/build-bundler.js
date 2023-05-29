import esbuild from 'esbuild';

await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "esm",
  nodePaths: ['../../../../node_modules'],
  entryPoints: {
    'index': '../../../build/engine/package-cache/obj/index.js',
  },
  outdir: '../../../build/engine/package-cache/lib/',
  outExtension: { '.js': '.mjs' },
  tsconfig: './tsconfig.json',
  target: "es5"
});

await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "esm",
  nodePaths: ['../../../../node_modules'],
  entryPoints: {
    'dom-cloud-requester': '../../../build/engine/package-cache/obj/domCloudRequester.js',
  },
  outdir: '../../../build/engine/package-cache/lib/',
  outExtension: { '.js': '.mjs' },
  tsconfig: './tsconfig.json',
  target: "es5"
});

await esbuild.build({
  bundle: true,
  sourcemap: true,
  format: "esm",
  nodePaths: ['../../../../node_modules'],
  entryPoints: {
    'node-cloud-requester': '../../../build/engine/package-cache/obj/nodeCloudRequester.js',
  },
  outdir: '../../../build/engine/package-cache/lib/',
  outExtension: { '.js': '.mjs' },
  platform: 'node',
  tsconfig: './tsconfig.json',
  target: "es5"
});