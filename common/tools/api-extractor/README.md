# Introduction

These files are used by `typescript_run_api_extractor()` in typescript.inc.sh.

This is setup only for Developer projects at this time as outputs go into
developer/docs and developer/build; future generalization requires changing only
`$report_temp` and `$report_folder` parameters in
`typescript_run_api_extractor()`.

## api-extractor.template.json

* Reference: https://api-extractor.com

api-extractor.template.json contains a template for api-extractor; these
parameters cannot be passed in to the tool, so we modify this template as needed
with the following parameters:

* `$keyman_root`: the `$KEYMAN_ROOT` variable, with backslash \ translated to
  forward slash /
* `$index_d_ts`: the filename `index.d.ts` or the corresponding filename for the
  entry point of the project
* `$project_path`: the path for the module, relative to the base of the repo
* `$report_temp`: a temporary path for output files for api-extractor
* `$report_folder`: target folder for completed api-extractor API documentation

## tsdoc.template.json

* Reference: https://tsdoc.org/pages/packages/tsdoc-config/

tsdoc.template.json is copied (unmodified) from this folder into tsdoc.json in
project folders (alongside tsconfig.json) before running api-extractor and
removed again afterwards; there is no way to specify an alternate location for
the file.

tsdoc.template.json includes a definition for "@since" which has been proposed
in https://github.com/microsoft/tsdoc/issues/136.

## Notes

See also .github/workflows/api-verification.yml which does API validation of
Core for desktop platforms.