/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * The LDML editor itself
 */

import * as vscode from 'vscode';
import * as crypto from 'crypto';

interface LdmlDocumentDelegate {
	getFileData(): Promise<Uint8Array>;
}

class LdmlDocument implements vscode.CustomDocument {
    constructor(public uri: vscode.Uri, public backupId: string | undefined, public delegate: LdmlDocumentDelegate) {

    }

    static create(uri: vscode.Uri, backupId: string | undefined, delegate: LdmlDocumentDelegate): LdmlDocument | PromiseLike<LdmlDocument> {
        return new LdmlDocument(uri, backupId, delegate);
    }

    private _onDidChangeDocument = new vscode.EventEmitter<{
        // TODO-LDML-EDITOR
	}>();

    public readonly onDidChangeContent = this._onDidChangeDocument.event;

    private _onDidDispose = new vscode.EventEmitter<{
        // TODO-LDML-EDITOR
	}>();

    public readonly onDidDispose = this._onDidDispose.event;

    dispose(): void {
        [this._onDidChangeDocument,
            this._onDidDispose].forEach(e => e.dispose());
        // TODO-LDML-EDITOR others
    }
}

export class LdmlEditorProvider implements vscode.CustomTextEditorProvider {
    private static readonly viewType = 'keyman.ldml'; // sync w/ package.json

    constructor(private readonly context: vscode.ExtensionContext) { }
	static register(context: vscode.ExtensionContext): vscode.Disposable {
        const provider = new LdmlEditorProvider(context);
        const providerRegistration = vscode.window.registerCustomEditorProvider(LdmlEditorProvider.viewType, provider);
        return providerRegistration;
	}

	private readonly _onDidChangeCustomDocument = new vscode.EventEmitter<vscode.CustomDocumentEditEvent<LdmlDocument>>();
	public readonly onDidChangeCustomDocument = this._onDidChangeCustomDocument.event;

    async saveCustomDocument(document: LdmlDocument, cancellation: vscode.CancellationToken): Promise<void> {
        // TODO-LDML-EDITOR
        return;
    }
    async saveCustomDocumentAs(document: LdmlDocument, destination: vscode.Uri, cancellation: vscode.CancellationToken): Promise<void> {
        // TODO-LDML-EDITOR
        return;
    }
    async revertCustomDocument(document: LdmlDocument, cancellation: vscode.CancellationToken): Promise<void> {
        // TODO-LDML-EDITOR
        return;
    }
    async backupCustomDocument(document: LdmlDocument, context: vscode.CustomDocumentBackupContext, cancellation: vscode.CancellationToken): Promise<vscode.CustomDocumentBackup> {
        throw new Error('Method not implemented.'); // TODO-LDML-EDITOR
    }
    async openCustomDocument(uri: vscode.Uri, openContext: vscode.CustomDocumentOpenContext, token: vscode.CancellationToken): Promise<LdmlDocument> {
        const document : LdmlDocument = await LdmlDocument.create(uri, openContext.backupId, {
            getFileData: async() => {
                return new Uint8Array(); // TODO-LDML-EDITOR
            }
        });

        const listeners: vscode.Disposable[] = [];

        // listeners.push(document.onDidChange(e => {
		// 	// Tell VS Code that the document has been edited by the use.
		// 	this._onDidChangeCustomDocument.fire({
		// 		document,
		// 		...e,
		// 	});
		// }));

        listeners.push(document.onDidChangeContent(e => {
            // TODO
        }));

        document.onDidDispose(() => listeners.forEach(e => e.dispose()));

        return document;

    }
    async resolveCustomTextEditor(document: vscode.TextDocument, webviewPanel: vscode.WebviewPanel, token: vscode.CancellationToken): Promise<void> {
        webviewPanel.webview.options = {
			enableScripts: true,
		};

        const { webview } = webviewPanel;

        const styleMainUri = webview.asWebviewUri(vscode.Uri.joinPath(this.context.extensionUri, 'media', 'ldml.css'));

        const nonce = crypto.randomUUID().toString();

        // TODO-LDML-EDITOR move to React function
        webview.html = `
            <!DOCTYPE html>
			<html lang="en">
			<head>
				<meta charset="UTF-8">


				<!--
				Use a content security policy to only allow loading images from https or from our extension directory,
				and only allow scripts that have a specific nonce.
				-->
				<meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src ${webview.cspSource}; style-src ${webview.cspSource}; script-src 'nonce-${nonce}';">

                <link href="${styleMainUri}" rel="stylesheet" />

                <meta name="viewport" content="width=device-width, initial-scale=1.0">

                <title>LDML</title>
            </head>
            <body>
                <h1>Hello, World! </h1>
                <pre>${document.getText().replaceAll('<', '&lt;').trim()}</pre>
            </body>
            </html>
        `.trim();

        function updateWebview() {
			webviewPanel.webview.postMessage({
				type: 'update',
				text: document.getText(),
			});
		}

		webviewPanel.webview.onDidReceiveMessage(e => {
			switch (e.type) {
                // TODO-LDML-EDITOR  actions from the UI to the extension
				// case 'add':
				// 	this.addNewScratch(document);
				// 	return;

				// case 'delete':
				// 	this.deleteScratch(document, e.id);
				// 	return;
			}
		});

        updateWebview();
    }
}
