/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import * as process from 'node:process';
import { CompilerCallbackOptions } from "@keymanapp/developer-utils";
import { NodeCompilerCallbacks } from "@keymanapp/kmc/build/src/util/NodeCompilerCallbacks.js";

export class ExtensionCallbacks extends NodeCompilerCallbacks {
    /**
     * @param options options to pass to NodeCompilerCallbacks
     * @param msg callback to write to the console
     */
    constructor(options: CompilerCallbackOptions, private msg?: (m: string)=>void) {
        super(options);
    }

    protected writeString(str: string) {
        if (this.msg) {
            this.msg(str);
        } else {
            process.stdout.write(str);
        }
    }
}
