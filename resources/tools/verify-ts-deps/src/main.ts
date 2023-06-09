import * as path from 'path';
import * as fs from 'fs';
import JSON5 from 'json5';
import * as readline from 'readline';
import { Command } from 'commander';

const program = new Command();

let moduleName: string;

/* Arguments */
program
  .description('Module Dependency Verification')
  .arguments('[module]')
  .action(module => moduleName = module);

program.parse(process.argv);

class Module {
  id: string;   // @keymanapp/common-types
  path: string; // relative to KEYMAN_ROOT, e.g. common/web/types
  deps: {
    inTsFiles: Set<string>;
    packageJson: Set<string>;
    typescriptRefs: Set<string>;
    typescriptPaths: Set<{path:string,id:string}>;
    buildSh: Set<string>;
  } = {
    inTsFiles: new Set<string>(),
    packageJson: new Set<string>(),
    typescriptRefs: new Set<string>(),
    typescriptPaths: new Set<{path:string,id:string}>(),
    buildSh: new Set<string>()
  };
  tsFiles: string[] = [];
};

let modules: Module[] = [];

/*
let ignores: string[] = [];

function refreshIgnores(root: string, currentPath: string) {
  ignore.default().add(fs.readFileSync(path.join(root, currentPath, '.gitignore'), 'utf-8'));
}

function skipIgnoredFile(root: string, currentPath: string, file: fs.Dirent) {

}
*/

function isFileGitIgnored(root: string, currentPath: string, filename: string) {
  // TODO: scan .gitignore files and use those instead of this mishmash
  if(filename == 'node_modules' || filename.startsWith('.')) {
    return true;
  }

  if(filename == 'build' || filename == 'bin' || filename == 'obj') {
    return true;
  }

  if(filename == 'packages' && currentPath.replace(/\\/g, '/') == 'windows/src/engine') {
    return true;
  }

  return false;
  // return child_process.spawnSync('git', ['check-ignore', '-q', filename]).status === 0;
}

function checkPath(root: string, currentPath: string, module: Module) {
  readline.clearLine(process.stdout, 0);
  readline.cursorTo(process.stdout, 0);
  process.stdout.write(('Checking path '+currentPath).substring(0, process.stdout.columns-1));
  let files = fs.readdirSync(path.join(root, currentPath), {withFileTypes: true});
  // if(files.find(file => file.name == '.gitignore')) {
  //   refreshIgnores(root, currentPath);
  // }
  // files = files.filter(file => skipIgnoredFile(root, currentPath, file));
  if(files.find(file => file.name == 'package.json')) {
    module = new Module();
    module.path = currentPath.replace(/\\/g, '/');
    modules.push(module);
    checkPackageJson(root, path.join(currentPath, 'package.json'), module);
  } else if (files.find(file => file.name == 'tsconfig.json')) {
    module = new Module();
    module.path = currentPath.replace(/\\/g, '/');
    modules.push(module);
    checkTSConfigJson(root, path.join(currentPath, 'tsconfig.json'), module);
  }

  for(let file of files) {
    if(isFileGitIgnored(root, currentPath, file.name)) {
      continue;
    }

    let fullFilename = path.join(currentPath, file.name);

    if(file.isDirectory()) {
      checkPath(root, fullFilename, module);
    } else if(file.name == 'tsconfig.json') {
      checkTSConfigJson(root, fullFilename, module);
    } else if(file.name.endsWith('.ts')) {
      checkTypescript(root, fullFilename, module);
    } else if(file.name == 'build.sh') {
      checkBuildSh(root, fullFilename, module);
    }
  }

}

function resolveFilename(baseFilename: string, filename: string) {
  const basePath =
    baseFilename.endsWith('/') || baseFilename.endsWith('\\') ?
    baseFilename :
    path.dirname(baseFilename);
  // Transform separators to platform separators -- we are agnostic
  // in our use here but path prefers files may use
  // either / or \, although older kps files were always \.
  if(path.sep == '/') {
    filename = filename.replace(/\\/g, '/');
  } else {
    filename = filename.replace(/\//g, '\\');
  }
  if(!path.isAbsolute(filename)) {
    filename = path.resolve(basePath, filename);
  }
  return filename;
}

function checkTSConfigJson(root: string, filename: string, module: Module) {
  const fullFilename = path.join(root, filename);
  let json = JSON5.parse(fs.readFileSync(fullFilename, 'utf-8'));
  if(json.references) {
    json.references.forEach( (ref:any) => {
      const refPath = path.relative(root, resolveFilename(fullFilename, ref.path)).replace(/\\/g, '/');
      module.deps.typescriptRefs.add(refPath);
    });
  }
  if(json.compilerOptions?.paths) {
    Object.keys(json.compilerOptions.paths).forEach( id => {
      // "@keymanapp/common-types": ["../../../common/web/types/src/main"]
      if(json.compilerOptions.paths[id].length != 1) {
        throw new Error(`Invalid tsconfig.json ${filename}: expected 1 array entry in compilerOptions.paths[${id}]`);
      }
      const refPath = path.relative(root, resolveFilename(fullFilename, json.compilerOptions.paths[id][0])).replace(/\\/g, '/');
      module.deps.typescriptPaths.add({id: id, path: refPath});
    });
  }
}

function checkPackageJson(root: string, filename: string, module: Module) {
  const fullFilename = path.join(root, filename);
  let json = JSON5.parse(fs.readFileSync(fullFilename, 'utf-8'));
  module.id = json.name;
  if(json.dependencies) {
    Object.keys(json.dependencies).forEach( key => {
      if(key.match(/^@keymanapp\//)) {
        module.deps.packageJson.add(key);
      }
    });
  }
  if(json.devDependencies) {
    Object.keys(json.devDependencies).forEach( key => {
      if(key.match(/^@keymanapp\//)) {
        module.deps.packageJson.add(key);
      }
    });
  }
}

function checkTypescript(root: string, filename: string, module: Module) {
  const fullFilename = path.join(root, filename);
  module.tsFiles.push(filename.replace(/\\/g, '/'));
  const lines = fs.readFileSync(fullFilename, 'utf-8').split('\n');
  for(let line of lines) {
    let res = line.match(/\s*import.+from\s+['"](@keymanapp\/[^'"]+)['"]/);
    if(res) {
      module.deps.inTsFiles.add(res[1]);
    }
  }
}

function checkBuildSh(root: string, filename: string, module: Module) {
  const fullFilename = path.join(root, filename);
  const lines = fs.readFileSync(fullFilename, 'utf-8').split('\n');
  let inDescribe = false, bd = '';
  for(let line of lines) {
    if(line.match('builder_describe')) {
      inDescribe = true;
    }
    if(inDescribe) {
      line = line.trim();
      if(line.endsWith('\\')) {
        line = line.substring(0, line.length - 1);
      } else {
        inDescribe = false;
      }
      bd = bd + line + ' ';
      if(!inDescribe) {
        break;
      }
    }
  }

  // console.log(bd);
  // if(filename == 'developer\\src\\kmc-package\\build.sh') //.includes('@'))
    // process.exit(0);

  let components = bd.split(' ');
  for(let component of components) {
    if(component.startsWith('"')) {
      component = component.substring(1);
    }
    if(component.startsWith('@')) {
      if(component.endsWith('"')) {
        component = component.substring(0, component.length-1);
      }
      if(component.startsWith('@/')) {
        component = component.substring(1);
      }
      component = component.substring(1);
      if(fs.existsSync(path.join(root, component, 'package.json')) ||
          fs.existsSync(path.join(root, component, 'tsconfig.json'))) {
        module.deps.buildSh.add(component);
      }
    }
  }
}

process.stdout.write('Checking folders');
checkPath('C:\\Projects\\keyman\\app', '', null);
readline.clearLine(process.stdout, 0);
readline.cursorTo(process.stdout, 0);
process.stdout.write('Finished checking folders\n');

if(moduleName) {
  let module = modules.find(module => module.id == moduleName);
  if(module) {
    verifyModule(module);
  }
  else {
    console.error(`Module ${moduleName} not found`);
    process.exit(1);
  }
} else {
  for(let module of modules) {
    verifyModule(module);
  }
}

function moduleIdToPath(id: string): string {
  let module = modules.find(module => module.id == id);
  if(module) {
    return module.path;
  }
  return null;
}

function modulePathToId(path: string): string {
  let module = modules.find(module => module.path == path);
  if(module) {
    return module.id;
  }
  return null;
}



function verifyModule(module: Module) {
  // console.dir(module, {depth: 4});

  let deps: {
    id?:string,
    path?:string,
    'package.json'?: boolean,
    'tsconfig.json/references'?: boolean,
    'tsconfig.json/paths'?: boolean,
    '.ts'?: boolean,
    'build.sh'?: boolean
  }[] = [];
  // We'll emit a grid of deps + usages, first
  for(let id of module.deps.inTsFiles) {
    deps.push({id, path:moduleIdToPath(id), '.ts': true});
  }
  for(let id of module.deps.packageJson) {
    let f = deps.find(d => d.id == id);
    if(!f) deps.push({id, path:moduleIdToPath(id), 'package.json': true});
    else f['package.json'] = true;
  }
  for(let path of module.deps.buildSh) {
    let f = deps.find(d => d.path == path);
    if(!f) deps.push({id:modulePathToId(path), path, 'build.sh': true});
    else f['build.sh'] = true;
  }
  for(let path of module.deps.typescriptRefs) {
    if(path == 'common/web/keyman-version/tsconfig.esm.json')
      path = 'common/web/keyman-version';
    let f = deps.find(d => d.path == path);
    if(!f) deps.push({id:modulePathToId(path), path, 'tsconfig.json/references': true});
    else f['tsconfig.json/references'] = true;
  }
  for(let {id, path} of module.deps.typescriptPaths) {
    let f = deps.find(d => d.id == id);
    if(!f) deps.push({id, path});
    else f['tsconfig.json/paths'] = true;
  }

  let table: any = {};
  console.log('='.repeat(process.stdout.columns));
  console.log('');
  console.log(`Module ID:         ${module.id}`);
  console.log(`Module Path:       ${module.path}`);
  console.log(`# .ts files found: ${module.tsFiles.length}`);

  let n = 0;
  for(let dep of deps) {
    let row: any = {path: dep.path};
    row['package.json'] = dep['package.json'] ? '✔' : null;
    row['tsconfig.json/references'] = dep['tsconfig.json/references'] ? '✔' : null;
    row['tsconfig.json/paths'] = dep['tsconfig.json/paths'] ? '✔' : null;
    row['.ts'] = dep['.ts'] ? '✔' : null;
    row['build.sh'] = dep['build.sh'] ? '✔' : null;
    table[dep.id ?? n] = row;
    n++;
  }
  console.table(table);
}

