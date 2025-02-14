/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { LDMLCompilerManager } from "./ldmlCompilerManager.js";
import { CompilerCallbackOptions, CompilerError, defaultCompilerOptions, LDMLKeyboardXMLSourceFileReader } from "@keymanapp/developer-utils";
import { LdmlCompilerOptions, LdmlKeyboardCompiler } from "@keymanapp/kmc-ldml";
import { ExtensionCallbacks } from "./extensionCallbacks.mjs";
import { KMXPlus } from "@keymanapp/common-types";
import { fileURLToPath } from 'url';
import { dirname } from 'node:path';

/**
 * TODO-LDML-EDITOR: should use vscode.workspace.fs.readFile which can read from remote workspaces.
 *
 * TODO-LDML-EDITOR: naming can be improved.
 */

export class KmcLdmlManager implements LDMLCompilerManager {
    calloptions?: CompilerCallbackOptions;
    callbacks?: ExtensionCallbacks;
    compoptions?: LdmlCompilerOptions;

    async init(): Promise<any> {
        if (this.callbacks) {
            return; // already initted
        }
        console.log("Initting KmcLdmlManager…");
        this.calloptions = {
            logLevel: "debug"
        };
        this.callbacks = new ExtensionCallbacks(this.calloptions); // TODO-EPIC-LDML: capture output
        console.log("KmcLdmlManager okay!");
        console.log('Initted');
    }

    async compile(filename: string): Promise<KMXPlus.KMXPlusFile> {
        if (!this.callbacks) {
            throw Error(`Must call init() first.`);
        }
        this.callbacks.clear(); // clear each time
        const k = new LdmlKeyboardCompiler();
        this.compoptions = {
            ...defaultCompilerOptions,
            readerOptions: {
                cldrImportsPath: fileURLToPath(new URL(...LDMLKeyboardXMLSourceFileReader.defaultImportsURL)),
                localImportsPaths: [ dirname(filename) ], // local dir
            }
        };
        await k.init(this.callbacks, this.compoptions);
        const source = await k.load(filename);
        console.log(`loaded ${filename}`);
        if (!source) {
            throw Error(`Could not load ${filename}`);
        }
        const compiled = await k.compile(source);
        if (!compiled) {
            throw Error(`Could not compile ${filename}`);
        }
        console.log(`compiled ${filename}`);
        return compiled;
    }

    getMessages() : string[] {
        if (!this.callbacks) {
            throw Error(`Must call init() first.`);
        }
        return this.callbacks.messages.map(m => CompilerError.formatEvent(m));
    }

};
