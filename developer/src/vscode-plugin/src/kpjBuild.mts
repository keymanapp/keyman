/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */


/**
 * Code related to building KPJ projects
 */

/**
 * TODO Some of this code may  look very familiar and should be refactored from kmc !
 */

import { platform } from "node:os";
import { extname, dirname, resolve } from "node:path";
import { mkdir } from "node:fs/promises";
import { KeymanFileTypes } from "@keymanapp/common-types";
import { KPJFileReader, CompilerCallbackOptions, CompilerOptions, LDMLKeyboardXMLSourceFileReader } from "@keymanapp/developer-utils";
import { ExtensionCallbacks } from "./extensionCallbacks.mjs";
import * as kmcLdml from '@keymanapp/kmc-ldml';
import * as kmcKmn from '@keymanapp/kmc-kmn';
import { fileURLToPath } from 'url';
import { KmpCompiler, KmpCompilerOptions } from "@keymanapp/kmc-package";


/**
 * Ensure that the parent dirs of filePath exist
 * @param filePath path to some file
 */
async function mkParentDirs(filePath: string) {
    const dir = dirname(filePath);
    await mkdir(dir, { recursive: true });
}

/**
 * Build a whole .kpj
 * @param workspaceRoot root dir for work
 * @param kpjPath  path to the .kpj
 * @param msg callback for writing messages
 * @returns accept on OK, otherwise throws
 */
export async function buildProject(workspaceRoot: string,
    kpjPath: string, msg: (m: string)=>void): Promise<void> {


    function getPosixAbsolutePath(filename: string): string {
        if (platform() == 'win32') {
            // On Win32, we need to use backslashes for path.resolve to work
            filename = filename.replace(/\//g, '\\');
        }
        const path = { resolve };
        // Resolve to a fully qualified absolute path relative to workspaceRoot
        filename = path.resolve(workspaceRoot, filename);

        if (platform() == 'win32') {
            // Ensure that we convert the result back to posix-style paths which is what
            // kmc-kmn expects. On posix platforms, we assume paths have forward slashes
            // already
            filename = filename.replace(/\\/g, '/');
        }
        return filename;
    }

    const callbacks = new ExtensionCallbacks({}, msg);

    // const outfile = '';
    const coptions : CompilerCallbackOptions = {
    };
    const options : CompilerOptions = {
    };
    // const callbacks = new NodeCompilerCallbacks(coptions);

    // const resp = await (new BuildProject().build(
    // 	infile,
    // 	<string><unknown>undefined,
    // 	callbacks,
    // 	options
    // ));

    // // dump all
    // callbacks.messages?.forEach(m => console.dir(m));
    // const resp = false;

    // return resp;

    msg(`Keyman Vancouver: Begin build of ${kpjPath}…\r\n`);

    const reader = new KPJFileReader(callbacks);

    const prj = reader.read(callbacks.fs.readFileSync(kpjPath));

    if (!prj) {
        msg(`Could not load ${kpjPath}\r\n`);
        return;
    }
    try {
        reader.validate(prj);
    } catch(e) {
        console.error(e);
        msg(`Error validating ${kpjPath}\r\n`);
    }

    // we don't need to see it.
    // msg(`PRJ loaded: ${JSON.stringify(prj, null, ' ')}\r\n`);

    // this next line is important - we need the full (?) project
    // otherwise we get an empty shell
    const project = await reader.transform(kpjPath, prj);
    // msg(`project loaded: ${JSON.stringify(project, null, ' ')}\r\n`);

    let didCompileSrc = false;
    let didCompilePkg = false;

    for (const path of project.files.filter(({ filePath }) => extname(filePath) === KeymanFileTypes.Source.LdmlKeyboard)) {
        const { filePath } = path;
        msg(`Compiling LDML: ${filePath}\r\n`);

        const ldmlCompilerOptions: kmcLdml.LdmlCompilerOptions = {
            ...options, readerOptions: {
                importsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL))
            }
        };
        const compiler = new kmcLdml.LdmlKeyboardCompiler();
        if (!await compiler.init(callbacks, ldmlCompilerOptions)) {
            msg(`Compiler failed init\r\n`);
            continue;
        }

        const outfile = project.resolveOutputFilePath(path, KeymanFileTypes.Source.LdmlKeyboard, KeymanFileTypes.Binary.Keyboard);
        msg(`.. outfile is ${outfile}\r\n`);
        await mkParentDirs(outfile);
        const result = await compiler.run(filePath, outfile);
        if (!result) {
            msg(`Compiler failed to run\r\n`);
            continue;
        }
        msg(`.. compiled\r\n`);
        // if(!this.createOutputFolder(outfile ?? infile, callbacks)) {
        //             return false;
        // }

        if (!await compiler.write(result.artifacts)) {
            msg(`Error writing ${outfile}\r\n`);
            throw Error(`Error writing ${outfile}`);
        }

        msg(`.. wrote\r\n`);

        msg(`\r\n\r\n`);
        didCompileSrc = true; // we allow more than one xmk in each package
    }

    // now, compile any .kmn
    for (const path of project.files.filter(({ filePath }) => extname(filePath) === KeymanFileTypes.Source.KeymanKeyboard)) {
        const { filePath } = path;
        msg(`Compiling KMN: ${filePath}\r\n`);

        const compiler = new kmcKmn.KmnCompiler();
        if (!await compiler.init(callbacks, coptions)) {
            msg(`Compiler failed init\r\n`);
            continue;
        }

        const outfile = project.resolveOutputFilePath(path, KeymanFileTypes.Source.KeymanKeyboard, KeymanFileTypes.Binary.Keyboard);
        msg(`.. outfile is ${outfile}\r\n`);
        const infilePosix = getPosixAbsolutePath(filePath);
        const outfilePosix = getPosixAbsolutePath(outfile);
        await mkParentDirs(outfilePosix);
        msg(`${infilePosix} => ${outfilePosix}\r\n`);
        const result = await compiler.run(infilePosix, outfilePosix);
        if (!result) {
            msg(`Compiler failed to run\r\n`);
            continue;
        }
        msg(`.. compiled\r\n`);
        // if(!this.createOutputFolder(outfile ?? infile, callbacks)) {
        //             return false;
        // }

        if (!await compiler.write(result.artifacts)) {
            msg(`Error writing ${outfile}\r\n`);
            throw Error(`Error writing ${outfile}`);
        }

        msg(`.. wrote\r\n`);

        msg(`\r\n\r\n`);
        didCompileSrc = true; // we allow more than one xmk in each package
    }

    // check errs and get out

    if(callbacks.hasFailureMessage(false)) {
        throw Error(`Error building ${kpjPath}`);
    }

    if (!didCompileSrc) {
        throw Error(`Error: no source files were compiled.`);
    }


    // now, any packaging
    for (const path of project.files.filter(({ filePath }) => extname(filePath) === KeymanFileTypes.Source.Package)) {
        if (didCompilePkg) {
            throw Error(`Error: two packages were encountered.`);
        }

        const { filePath } = path;
        const kmpCompilerOptions: KmpCompilerOptions = {
            ...options
        };
        const outfile = project.resolveOutputFilePath(path, KeymanFileTypes.Source.Package, KeymanFileTypes.Binary.Package);
        const infilePosix = getPosixAbsolutePath(filePath);
        const outfilePosix = getPosixAbsolutePath(outfile);
        await mkParentDirs(outfilePosix);
        msg(`Packaging: ${filePath} into ${outfile}\r\n`);

        const compiler = new KmpCompiler();
        if (!await compiler.init(callbacks, kmpCompilerOptions)) {
            msg(`Compiler failed init\r\n`);
            continue;
        }

        const result = await compiler.run(infilePosix, outfilePosix);
        if (!result) {
            msg(`Compiler failed to run\r\n`);
            continue;
        }
        msg(`.. compiled\r\n`);

        if (!await compiler.write(result.artifacts)) {
            msg(`Error writing ${outfile}\r\n`);
            throw Error(`Error writing ${outfile}`);
        }

        msg(`.. wrote\r\n`);

        msg(`\r\n\r\n`);
        didCompilePkg = true;
    }

    if (!didCompilePkg) {
        throw Error(`Error: no packages were compiled.`);
    }

    msg(`All done.\r\n`);
    return;
}
