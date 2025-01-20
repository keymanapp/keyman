/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * GitHub and Keyman Cloud interface wrappers
 */
import { CompilerCallbacks, GitHubUrls } from "@keymanapp/developer-utils";
import { CopierMessages } from "./copier-messages.js";
import { KeymanFileTypes } from "@keymanapp/common-types";

export class GitHubRef {
  public owner: string;
  public repo: string;
  public branch: string;
  public path: string;
  constructor(owner: string | GitHubRef | GitHubUrls.GitHubRegexMatchArray, repo?: string, branch?: string, path?: string) {
    if(typeof owner == 'string') {
      this.owner = owner;
      this.repo = repo;
      this.branch = branch;
      this.path = path;
    } else if("groups" in owner) {
      this.owner = owner.groups.owner;
      this.repo = owner.groups.repo;
      this.branch = owner.groups.branch;
      this.path = owner.groups.path;
    } else if("owner" in owner) {
      this.owner = owner.owner;
      this.repo = owner.repo;
      this.branch = owner.branch;
      this.path = owner.path;
    } else {
      throw new Error(`Unrecognized GitHubRef '${owner}'`)
    }
  }
  toString() {
    return (this.branch && this.path
      ? `${this.owner}/${this.repo}/tree/${this.branch}/${this.path}`
      : `${this.owner}/${this.repo}`
    );
  }
}

export class KeymanCloudSource {
  public static readonly KeymanApp_Owner = 'keymanapp';
  public static readonly KeymanApp_Keyboards_Repo = 'keyboards';
  public static readonly KeymanApp_Keyboards_DefaultBranch = 'master';
  public static readonly KeymanApp_LexicalModels_Repo = 'lexical-models';
  public static readonly KeymanApp_LexicalModels_DefaultBranch = 'master';

  constructor(private callbacks: CompilerCallbacks) {
  }

  public async getDefaultBranchFromGitHub(ref: GitHubRef): Promise<string> {
    const url = `https://api.github.com/repos/${ref.owner}/${ref.repo}`;
    let data: any;
    try {
      data = await this.callbacks.net.fetchJSON(url);
    } catch(e) {
      this.callbacks.reportMessage(CopierMessages.Error_CannotDownloadRepoFromGitHub({ref: ref.toString(), message: e?.message, cause: e?.cause?.message}));
      return null;
    }
    return data?.default_branch ?? null;
  }

  public async downloadFolderFromGitHub(ref: GitHubRef): Promise<{filename:string,type:'dir'|'file'}[]> {
    // this.callbacks.reportMessage(CopierMessages.Info_DownloadingFile({ref})); // TODO-COPY: verbose mode support

    const url = `https://api.github.com/repos/${ref.owner}/${ref.repo}/contents/${ref.path}?ref=${ref.branch}`;
    let folder: any;
    try {
      folder = await this.callbacks.net.fetchJSON(url);
    } catch(e) {
      this.callbacks.reportMessage(CopierMessages.Error_CannotDownloadFolderFromGitHub({ref: ref.toString(), message: e?.message, cause: e?.cause?.message}));
      return null;
    }

    if(!Array.isArray(folder)) {
      this.callbacks.reportMessage(CopierMessages.Error_FolderDownloadedFromGitHubIsNotAValidFolder({ref: ref.toString()}));
      return null;
    }

    return folder.map(item => ({
      filename: item.name,
      type: item.type
    }));
  }

  public async downloadFileFromGitHub(ref: GitHubRef): Promise<Uint8Array> {
    ref = new GitHubRef(ref); // so we can change props without mutating original
    ref.path = ref.path.startsWith('/') ? ref.path : '/' + ref.path;
    if(!ref.branch || ref.branch == '') {
      // TODO: lookup default branch
      ref.branch = 'master';
    }

    const url = `https://raw.githubusercontent.com/${ref.owner}/${ref.repo}/refs/heads/${ref.branch}${ref.path}`;

    try {
      return await this.callbacks.net.fetchBlob(url);
    } catch(e) {
      if(KeymanFileTypes.binaryTypeFromFilename(this.callbacks.path.extname(ref.path)) != null) {
        // We don't really expect .kmx, .js, etc to be in the repo, so just give informational message
        this.callbacks.reportMessage(CopierMessages.Info_CannotDownloadBinaryFileFromGitHub({ref: ref.toString(), message: e?.message, cause: e?.cause?.message}));
      } else {
        this.callbacks.reportMessage(CopierMessages.Warn_CannotDownloadFileFromGitHub({ref: ref.toString(), message: e?.message, cause: e?.cause?.message}));
      }
      return null;
    }
  }

  public async getSourceFromKeymanCloud(id: string, isModel: boolean): Promise<GitHubRef | null> {
    if(!id.match(/^[a-z0-9_]+/)) {
      this.callbacks.reportMessage(CopierMessages.Error_InvalidCloudKeyboardId({id}));
      return null;
    }

    const url = `https://api.keyman.com/${isModel ? 'model' : 'keyboard'}/${id}`;
    let json: any;

    try {
      json = await this.callbacks.net.fetchJSON(url);
    } catch(e) {
      this.callbacks.reportMessage(CopierMessages.Error_CouldNotRetrieveFromCloud({id, message: e?.message, cause: e?.cause?.message}));
      return null;
    }

    if(!json || typeof json != 'object') {
      this.callbacks.reportMessage(CopierMessages.Error_KeymanCloudReturnedInvalidData({id}));
      return null;
    }

    if(!json?.sourcePath) {
      // no source provided for the keyboard or model
      this.callbacks.reportMessage(CopierMessages.Error_CloudDoesNotHaveSource({id}));
      return null;
    }

    // One day, we'll support arbitrary sources once the api endpoint starts returning them,
    // but for now always keymanapp/keyboards|lexical-models. See keymanapp/api.keyman.com#260

    const ref: GitHubRef = new GitHubRef({
      branch: isModel ? KeymanCloudSource.KeymanApp_LexicalModels_DefaultBranch : KeymanCloudSource.KeymanApp_Keyboards_DefaultBranch,
      owner: KeymanCloudSource.KeymanApp_Owner,
      repo: isModel ? KeymanCloudSource.KeymanApp_LexicalModels_Repo : KeymanCloudSource.KeymanApp_Keyboards_Repo,
      path: (json.sourcePath.startsWith('/') ? '' : '/') + json.sourcePath
    });

    return ref;
  }
}

