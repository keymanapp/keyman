@echo off

rem excluded files from tests:

rem no *bmp file included in Folder
rem		- ekwtamil99uni

rem used other folders than /source
rem		- vn_telex/raw/header
rem		- vn_vni/raw/header

rem need more time to prevent TIMEOUT ( run while debugging in VS)
rem		- vietnamese_telex
rem		- vietnamese_telex_legacy
rem		- vietnamese_vni

rem used invalid characters
rem		- syriac_arabic
rem		- sil_hebr_grek_trans
rem		- sil_pan_africa_mnemonic
rem		- sil_pan_africa_positional
rem		- sil_yoruba8

dir  /s/b ..\..\..\..\keyboards\release\*.kmn | more |  findstr /v /i  "eKwTamil99UniUpdt" |  findstr /v /i  "viet*"  |  findstr /v /i  "syriac_arabic"  |  findstr /v /i  "sil_hebr_grek_trans"  |  findstr /v /i  "sil_pan_africa_mnemonic"  |  findstr /v /i  "sil_pan_africa_positional"  |  findstr /v /i  "sil_yoruba8"  
