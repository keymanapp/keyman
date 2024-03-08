export class KeymanUrls {
  static readonly HELP_KEYMAN_COM = `https://help.keyman.com`;
  static readonly KEYMAN_COM = `https://keyman.com`;

  // Various Sites
  static readonly NEW_KEYMAN_ISSUE = ()       => 'https://github.com/keymanapp/keyman/issues/new';
  static readonly LDML_SPEC = (topic: string) => `https://www.unicode.org/reports/tr35/tr35-keyboards.html#${topic}`;

  // TODO: Not formatting code here to avoid dep on @keymanapp/common-types for developer-utils
  static readonly COMPILER_ERROR_CODE = (code: string) => `https://kmn.sh/${code}`; // code should be km##### (already formatted)

  // help.keyman.com
  static readonly VIRTUAL_KEYS = ()         => `${KeymanUrls.HELP_KEYMAN_COM}/developer/language/guide/virtual-keys#common-virtual-key-codes`;
  static readonly FILE_TYPE = (ext: string) => `${KeymanUrls.HELP_KEYMAN_COM}/developer/current-version/reference/file-types/${ext}`;
  static readonly KMN_REF = (topic: string) => `${KeymanUrls.HELP_KEYMAN_COM}/developer/language/reference/${topic}`;
  static readonly HELP_KEYBOARD = (id: string)   => `${KeymanUrls.HELP_KEYMAN_COM}/keyboard/${id}`;
  static readonly HELP_MODEL = (id: string)      => `${KeymanUrls.HELP_KEYMAN_COM}/model/${id}`;


  // keyman.com
  static readonly KeymanDeveloper_KeymanForAndroidDownload = (version: string) => `${KeymanUrls.KEYMAN_COM}/go/developer/${version}/android-app`;
  static readonly KeymanDeveloper_KeymanForIosDownload     = (version: string) => `${KeymanUrls.KEYMAN_COM}/go/developer/${version}/ios-app`;

}
