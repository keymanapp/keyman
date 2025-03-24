/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Wrapper for network callbacks to give access to GitHub and Keyman Cloud URLs
 */
import { CompilerFileSystemAsyncCallbacks, CompilerPathCallbacks, FileSystemFolderEntry, CompilerCallbacks, UrlSubpathCompilerCallback, CompilerNetAsyncCallbacks, CompilerAsyncCallbacks } from "@keymanapp/developer-utils";
import { GitHubRef, KeymanCloudSource } from "./cloud.js";

export class CopierAsyncCallbacks implements CompilerAsyncCallbacks {
  public readonly path: CompilerPathCallbacks;
  public readonly fsAsync: CompilerFileSystemAsyncCallbacks;
  public readonly net: CompilerNetAsyncCallbacks;
  constructor(public readonly parent: CompilerCallbacks, public readonly githubRef: GitHubRef) {
    this.path = githubRef ? new UrlSubpathCompilerCallback() : parent.path;
    this.fsAsync = new CopierFileSystemAsyncCallbacks(this);
    this.net = parent.net;
  }
}

export class CopierFileSystemAsyncCallbacks implements CompilerFileSystemAsyncCallbacks {
  private cache: Record<string, {
    type: 'file' | 'dir';
    size: number;
    data?: Uint8Array;
    files?: FileSystemFolderEntry[];
  }> = {};
  private readonly keyboardCloudSource: KeymanCloudSource;

  constructor(private owner: CopierAsyncCallbacks)  {
    this.keyboardCloudSource = new KeymanCloudSource(owner.parent);
  }

  async readFile(filename: string): Promise<Uint8Array> {
    if (Object.hasOwn(this.cache, filename)) {
      return this.cache[filename].data;
    }

    const result: Uint8Array = this.owner.githubRef
      ? await this.keyboardCloudSource.downloadFileFromGitHub({...this.owner.githubRef, path: filename})
      : this.owner.parent.loadFile(filename);

    this.cache[filename] = {
      size: result?.byteLength ?? null,
      type: result ? 'file' : null,
      data: result,
      files: null
    };

    return result;
  }

  async readdir(path: string): Promise<FileSystemFolderEntry[]> {
    if (Object.hasOwn(this.cache, path)) {
      return this.cache[path].files;
    }

    let result: FileSystemFolderEntry[] = null;
    if (this.owner.githubRef) {
      const folder = await this.keyboardCloudSource.downloadFolderFromGitHub({...this.owner.githubRef, path});
      if (folder) {
        result = folder.map(item => ({
          filename: this.owner.path.basename(item.filename),
          type: item.type
        }));
      }
    } else {
      const dir = this.owner.parent.fs.readdirSync(path);
      if (dir) {
        result = dir.map(item => ({
          filename: item,
          type: this.owner.parent.isDirectory(this.owner.path.join(path, item)) ? 'dir' : 'file'
        }));
      }
    }

    this.cache[path] = {
      type: 'dir',
      files: result,
      data: null,
      size: null,
    };

    return result;
  }

  async exists(filename: string): Promise<boolean> {
    if (Object.hasOwn(this.cache, filename)) {
      return this.cache[filename].type == 'file'
        ? (this.cache[filename].data != null)
        : (this.cache[filename].files != null);
    }

    const path = this.owner.path.dirname(filename);
    if (Object.hasOwn(this.cache, path) && this.cache[path].files) {
      return this.cache[path].files.find(file => file.filename == filename) != null;
    }

    if (this.owner.githubRef) {
      const files = await this.readdir(path);
      const base = this.owner.path.basename(filename);
      return files != null && files.find(file => file.filename == base) != null;
    } else {
      if (this.owner.parent.isDirectory(filename)) {
        return true;
      }
    }

    return await this.readFile(filename) != null;
  }

  resolveFilename(baseFilename: string, filename: string) {
    baseFilename = baseFilename.replaceAll(/\\/g, '/');
    filename = filename.replaceAll(/\\/g, '/');
    const basePath = baseFilename.endsWith('/') ?
      baseFilename :
      this.owner.path.dirname(baseFilename);
    // Transform separators to platform separators -- we are agnostic
    // in our use here but path prefers files may use
    // either / or \, although older kps files were always \.
    if (!this.owner.path.isAbsolute(filename)) {
      filename = this.owner.path.resolve(basePath, filename);
    }
    return filename;
  }
}
