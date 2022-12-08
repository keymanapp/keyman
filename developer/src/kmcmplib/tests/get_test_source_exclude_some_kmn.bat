@echo off
rem Output a list of files to test, excluding some files

rem excluded files from tests:

rem no *bmp file included in Folder
rem		- eKwTamil99UniUpdt

rem use of other folders than /source
rem		- vietnamese_telex_legacy\extras\vn_kmn_compiler\vn_telex\raw
rem		- vietnamese_vni\extras\vn_kmn_compiler\keyboard\vn_vni\raw

rem need more time to prevent TIMEOUT ( run while debugging in VS)
rem		- vietnamese_telex
rem		- vietnamese_telex_legacy
rem		- vietnamese_vni

rem added CERR_XXXX-Tests to test if right ErrorMessage is sent

dir  /s/b ..\..\..\..\common\test\keyboards\invalid\*.kmn
dir  /s/b ..\..\..\..\..\keyboards\release\*.kmn | more |   findstr /v /i  "viet*" |  findstr /v /i  "eKwTamil99UniUpdt"
