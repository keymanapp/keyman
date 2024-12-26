/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * The LDML editor itself
 */

import * as vscode from 'vscode';
import * as crypto from 'crypto';
import { getLDMLCompilerManager, LDMLCompilerManager } from './ldmlCompilerManager';
import { fixupIndexHtml } from './indexFixer';

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
    private compiler? : LDMLCompilerManager;
    private static readonly viewType = 'keyman.ldml'; // sync w/ package.json

    private async getCompiler() : Promise<LDMLCompilerManager> {
        if (!this.compiler) {
            this.compiler = await getLDMLCompilerManager();
        }
        await this.compiler.init();
        return this.compiler;
    }

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
        console.log(`openCustom ${uri}`);
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
        // make sure we have a compiler
        const compiler = await this.getCompiler();
        // temporary - testing linkage
        const kmxPlus = await compiler.compile(document.fileName);

        webviewPanel.webview.options = {
			enableScripts: true,
		};

        const { webview } = webviewPanel;


        try {
            // we read this file, and munge it a bit.
            const indexUri = vscode.Uri.joinPath(this.context.extensionUri, 'build', 'index.html');
            // pass this as an argument to fixup URLs
            const buildUri = webview.asWebviewUri(vscode.Uri.joinPath(this.context.extensionUri, 'build'));

            const nonce = crypto.randomUUID().toString();

            console.dir({indexUri});
            const indexRaw = await vscode.workspace.fs.readFile(indexUri);
            const indexText = new TextDecoder('UTF-8').decode(indexRaw);
            const html = fixupIndexHtml(indexText, buildUri.toString(), nonce, webview.cspSource);
            console.log(html);
            webview.html = html;
        } catch(e) {
            console.error(e);
            webview.html = `<i>error: ${e}</i>`;
        }

        function updateWebview() {
			webviewPanel.webview.postMessage({
				type: 'update',
				text: document.getText(),
                kmxPlus,
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
