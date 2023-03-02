# kpj.xsd

**Note:** `KeymanDeveloperProject.Options.Version` is currently implicitly
always '1.0'. It will be required for version 2.0 and later of the format.

## 2023-02-27 2.0
* Version 2.0 makes 'Files' optional (internally, Files/File will be ignored,
  deleted on load and populated from folder structure). Adds Options/SourcePath,
  which defaults to '$PROJECTPATH/source', and Options/BuildPath now defaults to
  '$PROJECTPATH/build'.

## 2023-02-25 1.0
* Initial version 1.0, based on Delphi classes Keyman.Developer.System.Project.*
