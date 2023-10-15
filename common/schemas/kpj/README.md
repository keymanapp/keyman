# kpj.xsd

**Note:** `KeymanDeveloperProject.Options.Version` is currently implicitly
always '1.0'. It will be required for version 2.0 and later of the format.

**Note:** An additional schema file, kpj-9.0.schema.json, for supporting legacy
  versions of .kpj, from Keyman Developer 9.0 and earlier, is now available.

## 2023-08-07 2.0.1
* Add Options/SkipMetadataFiles, defaults to True for 1.0 projects.

## 2023-02-27 2.0
* Version 2.0 makes 'Files' optional (internally, Files/File will be ignored,
  deleted on load and populated from folder structure). Adds Options/SourcePath,
  which defaults to '$PROJECTPATH/source', and Options/BuildPath now defaults to
  '$PROJECTPATH/build'.

## 2023-02-25 1.0
* Initial version 1.0, based on Delphi classes Keyman.Developer.System.Project.*
