/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import * as vscode from 'vscode';
import { LdmlEditorProvider } from './ldmleditor';

/** called when extension is activated */
export function activate(context: vscode.ExtensionContext) {
  // LDML EDITOR STUFF
	context.subscriptions.push(LdmlEditorProvider.register(context));
}

/** called when extension is deactivated */
export function deactivate() {
}
