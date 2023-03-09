@echo off
rem Output a list of files to test, excluding some files

rem excluded files from tests:

rem no *bmp file included in Folder
rem		- eKwTamil99UniUpdt

rem use of other folders than \source
rem		- vietnamese_telex_legacy\extras\vn_kmn_compiler\vn_telex\raw
rem		- vietnamese_vni\extras\vn_kmn_compiler\keyboard\vn_vni\raw

rem need more time to prevent TIMEOUT (runs while debugging in VS)
rem		- vietnamese_telex
rem		- vietnamese_telex_legacy
rem		- vietnamese_vni

rem added CERR_XXXX-Tests to test if right ErrorMessage is sent

rem running under cmd under meson under bash causes pain:
set KR=%KEYMAN_ROOT:/=\%

dir  /s/b %KR%\common\test\keyboards\invalid\*.kmn
dir  /s/b %KR%\..\keyboards\release\*.kmn |   findstr /v /i  "viet*" |  findstr /v /i  "eKwTamil99UniUpdt" | findstr /c:"\\source\\"
