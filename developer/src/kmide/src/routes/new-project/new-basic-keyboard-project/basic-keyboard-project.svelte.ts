export type KeymanTarget = 'any' |
  'windows' | 'macosx' | 'linux' |
  'web' | 'iphone' | 'ipad' | 'androidphone' | 'androidtablet' |
  'mobile' | 'desktop' | 'tablet';

export type KeymanTargets = KeymanTarget[];

export interface IBasicKeyboardProject {
  copyright?: string;
  fullCopyright?: string;
  version?: string;
  author?: string;
  targets?: string;
  languages?: string;
  basePath?: string;
  description?: string;
  keyboardName?: string;
  keyboardID?: string;
};

export class BasicKeyboardProjectDataModel implements IBasicKeyboardProject {
  copyright?: string;
  fullCopyright?: string;
  version?: string;
  author?: string;
  targets?: string;
  languages?: string;
  basePath?: string;
  description?: string;
  keyboardName?: string;
  keyboardID?: string;
};

export class BasicKeyboardProject implements IBasicKeyboardProject {
  copyright?: string = $state('');
  fullCopyright?: string = $state('');
  version?: string = $state('1.0');
  author?: string = $state('');
  targets?: string = $state('');
  languages?: string = $state('');
  basePath?: string = $state('');
  description?: string = $state('');
  keyboardName?: string = $state('');
  keyboardID?: string = $state('');
};
