export interface ModelInfoFile {
  id?: string;
  name?: string;
  authorName?: string;
  authorEmail?: string;
  description?: string;
  license?: "mit";
  languages: Array<string>;
  lastModifiedDate?: string;
  packageFilename?: string;
  packageFileSize?: number;
  jsFilename?: string;
  jsFileSize?: number;
  isRTL?: boolean;
  packageIncludes?: string[]; //['fonts'] or []
  version?: string;
  minKeymanVersion?: string;
  helpLink?: string;
  sourcePath?: string;
  related?: {[id:string]:ModelInfoFileRelated};
}

export interface ModelInfoFileRelated {
  deprecates?: string;
  deprecatedBy?: string;
}