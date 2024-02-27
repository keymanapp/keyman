
This is a proposal to rewrite  mcompile for Linux.  For this we need to  query the base keyboard data from the Linux platform, then rewriting the keyboard .kmx using the same approach as is done in mcompile for Windows, but working from the data from the x11 keyboard on Linux.

Ideally, we'd rewrite mcompile to be cross-platform (Windows, Linux, macOS), so that the keyboard interrogation would be separated from the .kmx rewriting, at least to some degree. Nevertheless it would probably be easiest to start from a standalone implementation. 
Sample program that reads US basic keyboard and compares to key value group

# Keymap

__S2 TODO check if I can use files from some other keyman path instead of a copy here ( e.g. filesystem.h exists elsewhere); where can I use incxstr from
_S2 TODO Do I need HKL for Linux / can I just use a void* or remove HKL ??,  typeddef of KMX_HKL - can I delete all m_hkl from classes?
_S2 TODO Check/find use of wchar_t/wstring and replace with char16_t/u16string
_S2 TODO check call by reference/value
_S2 TODO what is wrong with kp->dpBitmapOffset/BitmapSize ?
_s2 INFO idee spanish keyboard has dk on altgr !!
_S2 TODO new Problem: RALT-dk(4): Why do we havea RALT in dk 4? It makes sense but how do we handle this??

_S2 TOP_1 NCAPS/capslock-> we need capslock-equation. ---> new Problem: RALT-dk(4)  RALT+Ä
_S2 TOP_2 Shiftflags
_S2 TOP_3 dk-table from file->from functions
_S2 TOP_4 HKL
_S2 TOP_5 use files/functions from other places
_S2 TOP_6 remaining


./mcompile -d /Projects/keyman/keyman/linux/mcompile/keymap/mcompile_test.kmx bla.dll 0407 /Projects/keyman/keyman/linux/mcompile/keymap/mcompile_test_out.kmx

./mcompile -d     /home/mcompileTest/Source/mcompile_test.kmx bla.dll 0407 /home/mcompileTest/Source_after_run_mcompile/mcompile_test_out.kmx
