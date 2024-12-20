/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

// import { NodeCompilerCallbacks } from "@keymanapp/kmc";
import { LDMLCompilerManager } from "./ldmlCompilerManager.js";

/**
 * TODO-LDML-EDITOR: should use vscode.workspace.fs.readFile which can read from remote workspaces.
 */

export class KmcLdmlManager implements LDMLCompilerManager {
    async init(): Promise<any> {
        console.log("Initting KmcLdmlManager…");
        // new NodeCompilerCallbacks({
        // });
        console.log("KmcLdmlManager okay!");
    }

};
