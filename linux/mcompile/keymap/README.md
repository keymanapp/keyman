
This is a proposal to rewrite  mcompile for Linux.  For this we need to  query the base keyboard data from the Linux platform, then rewriting the keyboard .kmx using the same approach as is done in mcompile for Windows, but working from the data from the x11 keyboard on Linux.

Ideally, we'd rewrite mcompile to be cross-platform (Windows, Linux, macOS), so that the keyboard interrogation would be separated from the .kmx rewriting, at least to some degree. Nevertheless it would probably be easiest to start from a standalone implementation. 
Sample program that reads US basic keyboard and compares to key value group

# Keymap

TODO check if US basic is the right Keyboard to compare with
TODO deadkeys don't work yet
TODO path for xkb/symbols as compile time option in meson
TODO define folder to store File_US.txt" in and find better name
TODO check if I can use files from some other keyman path instead of a copy in keymap ( e.g. filesystem.h exists elsewhere)
TODO shiftstate-count
TODO keymap SplitToV.cpp exchange 4 with genaerated shiftcount
TODO shift-statevector
TODO Do I need HKL for Linux / can I just use a void* or remove HKL ??
TODO typeddef of KMX_HKL - can I delete all m_hkl from classes?
TODO check if passed All_Vectpor as ptr/ref not as value e.g. in KMX_CharFromVK, KMX_VKUSToVKUnderlyingLayout, KMX_ImportRules
ToDo in get_VirtualKey_Other_From_SC:  what if we use  column 3(altgr) and 4 (shift+altgr) ??
TODO where to store VK_CANCEL.... in km_types.h or elsewhere?
ToDo check up to 8 shiftstates ( find symbols-file with 8)
ToDo use _free(keyvals);   g_free(maps);
ToDo use u16string OR wstring-functions only and remove the other

TODO next:
    - check if I use char16_t everywhere instead of wchar_t or char
    - Check/find use of wchar_t/wstring and replace with char16_t/u16string
    - replace GDK
    - what is wrong with kp->dpBitmapOffset/BitmapSize ?
    - check call by reference/value
TODO ...


./mcompile -d /Projects/keyman/keyman/linux/mcompile/keymap/mcompile_test.kmx bla.dll 0407 /Projects/keyman/keyman/linux/mcompile/keymap/mcompile_test_out.kmx

./mcompile -d     /home/mcompileTest/Source/mcompile_test.kmx bla.dll 0407 /home/mcompileTest/Source_after_run_mcompile/mcompile_test_out.kmx