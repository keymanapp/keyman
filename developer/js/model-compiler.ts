import * as TypeScript from 'typescript';
import * as fs from 'fs';
import * as assert from 'assert';

let sourceCode = fs.readFileSync(process.argv[2], 'utf8');

let compilation = TypeScript.transpileModule(sourceCode, {
compilerOptions: { module: TypeScript.ModuleKind.CommonJS }
})
let moduleCode = '(function(exports){'+compilation.outputText+'})'
console.log(moduleCode);
let fn = eval(moduleCode);
let o = {};
fn(o);
assert.strictEqual(o['__esModule'], true);
console.log(o['default']);