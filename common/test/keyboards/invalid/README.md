#-----------------------------------------
# Tests for kmcompxtest()
#-----------------------------------------

This folder contains several kmn-files that are used for testing resulting Errorcodes.
These tests are all copied from balochi_phonetic and contain alterations of the data to produce (at least) 1 Error.

#-----------------------------------------
#  Naming Convention:
#      CERR_404D_balochi_phonetic.kmn
#-----------------------------------------

The naming convention of the files is as follows:
All Files MUST start with CERR_ followed by 4 characters and an underscore.
After the second underscore all combination of char can be used.

The 4 characters following CERR_ correspond to the last 4 digits of the Error-Code which is expected to be produced by this file.
( e.g. CERR_404D_balochi_phonetic.kmn should produce Error 0x0000404D ).

#-----------------------------------------
While running kmcompxtest extracts the 4 chars of the Filename and compares them to the actual Error found.
If they correspond the test will be marked as OK.
If the file is supposed to produce an Error and does not the test will be marked as FAILED
If more than 1 Error is produced only the Error coded in the Filename will be detected
#-----------------------------------------



