@echo off

rem excluded files from tests:

rem no *bmp file included in Folder
rem		- eKwTamil99UniUpdt

rem use of other folders than /source
rem		- vn_telex/raw/header
rem		- vn_vni/raw/header

rem need more time to prevent TIMEOUT ( run while debugging in VS)
rem		- vietnamese_telex
rem		- vietnamese_telex_legacy
rem		- vietnamese_vni

rem added CERR_XXXX-Tests to test if right ErrorMessage is sent

dir  /s/b ..\..\..\common\test\keyboards\kmcompx_tests\*.kmn
dir  /s/b ..\..\..\..\keyboards\release\*.kmn | more |    findstr /v /i  "viet*" |  findstr /v /i  "eKwTamil99UniUpdt"

