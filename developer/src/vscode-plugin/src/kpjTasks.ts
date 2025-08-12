/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Code related to Tasks that build KPJ projects
 */

const path = require('node:path'); // node:path
const { existsSync } = require('node:fs');
import * as vscode from 'vscode';

/** promise to list of tasks */
let kpjPromise: Promise<vscode.Task[]> | undefined = undefined;

/** Task for kpj */
interface KpjTaskDefinition extends vscode.TaskDefinition {
    // empty for now
}

class KpjBuildTerminal implements vscode.Pseudoterminal {
    private writeEmitter = new vscode.EventEmitter<string>();
    onDidWrite: vscode.Event<string> = this.writeEmitter.event;
    private closeEmitter = new vscode.EventEmitter<number>();
    onDidClose?: vscode.Event<number> = this.closeEmitter.event;

    private fileWatcher: vscode.FileSystemWatcher | undefined;

    constructor(private workspaceRoot: string, private kpjPath: string, private flags: string[]) {
    }

    open(initialDimensions: vscode.TerminalDimensions | undefined): void {
        // At this point we can start using the terminal.
        if (this.flags.indexOf('watch') > -1) {
            const pattern = path.join(this.workspaceRoot, 'customBuildFile');
            this.fileWatcher = vscode.workspace.createFileSystemWatcher(pattern);
            this.fileWatcher.onDidChange(() => this.doBuild());
            this.fileWatcher.onDidCreate(() => this.doBuild());
            this.fileWatcher.onDidDelete(() => this.doBuild());
        }
        this.doBuild();
    }


    close(): void {
        // The terminal has been closed. Shutdown the build.
        if (this.fileWatcher) {
            this.fileWatcher.dispose();
        }
    }

    private async doBuild(): Promise<void> {
        this.writeEmitter.fire(`Starting build of ${this.kpjPath}...\r\n`);
        // esm so we can access keyman
        const { buildProject } = await import('./kpjBuild.mjs');
        try {
            await buildProject(this.workspaceRoot, this.kpjPath, this.writeEmitter.fire.bind(this.writeEmitter));
            this.closeEmitter.fire(0);
        } catch (e) {
            this.writeEmitter.fire(`Failure: ${e}\r\n\r\n`);
            this.closeEmitter.fire(1);
        }
        // TODO: get/set sharedState
    }
}


async function getKpjTasks(): Promise<vscode.Task[]> {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    const result: vscode.Task[] = [];
    if (!workspaceFolders || workspaceFolders.length === 0) {
        console.log('Keyman: No workspace folders');
        return result;
    }
    for (const workspaceFolder of workspaceFolders) {
        const folderString = workspaceFolder.uri.fsPath;
        if (!folderString) {
            console.log('Keyman: No folderString');
            continue;
        }
        const dir = path.basename(folderString);
        const kpjFile = path.join(folderString, `${dir}.kpj`);
        if (!existsSync(kpjFile)) {
            console.log(`Keyman: No kpj file ${kpjFile}`);
            continue;
        } else {
            console.log(`Keyman: Found kpj => ${kpjFile}`);
        }
        const task = new vscode.Task({ type: 'kpj' }, workspaceFolder, dir, 'kpj',
            new vscode.CustomExecution(async () => new KpjBuildTerminal(folderString, kpjFile, []))
            // new vscode.ShellExecution(`npx -y @keymanapp/kmc build file ${kpjFile}`)
        );
        task.group = vscode.TaskGroup.Build;
        result.push(task);
    }
    return result;
}

export const KpjTaskProvider = {

    // TODO should be TaskProvider subclass?

    provideTasks() {
        kpjPromise = kpjPromise ?? getKpjTasks();
        kpjPromise.catch(e => {
            console.error(e);
            // print something
            vscode.window.showErrorMessage(`Keyman: Error getting tasks: ${e}`);
        });
        return kpjPromise;
    },
    resolveTask(_task: vscode.Task): vscode.Task | undefined {
        const task = _task.definition.task;
        if (task) {
            const definition: KpjTaskDefinition = <any>_task.definition;
            return new vscode.Task(
                definition,
                _task.scope ?? vscode.TaskScope.Workspace,
                definition.type, // for now, we don't have another name to use
                'kpj',
                new vscode.CustomExecution(
                    // TODO: Hmm. This doesn't seem to be used??
                    async (): Promise<vscode.Pseudoterminal> => {
                        return new KpjBuildTerminal("something", "something/something.kpj", []);
                    }
                )
                // OLD: shell
                // new vscode.ShellExecution(`npx -y @keymanapp/kmc build`) // nothing from the definition for now
            );
        }
        return undefined;
    }
};
