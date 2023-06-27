# Keymap

Sample program that reads US basic keyboard and compares to key value group



TODO check if US basic is the right Keyboard to compare with
TODO non-letter characters don't work OK yet
TODO Umlauts don't work OK yet
TODO check Keycode of TLDE, BKSL, LSGT
TODO remove unnecessary printf/cout
TODO path for xkb/symbols as compile time option in meson
TODO check how many/which shift states we use ( at the moment we read all shiftstate-columns of US but then use only 2 colums (non-shift + shift) then use as many colums for Other )

TODO define folder to store File_US.txt" in and find better name
TODO get rid of GTK functions that are deprecated and use X11 instead
TODO retrieve name of Other keyboard and use appropriate name instead of "Other"
TODO change keymap.cpp->main()  to function()
TODO use/adapt TranslateKeyboard() to work on Linux/cross-platform
TODO use/adapt LoadKeyboard() to work on Linux/cross-platform
TODO use/adapt SaveKeyboard() to work on Linux/cross-platform
TODO include deadkeys
TODO mcompile.cpp: open mcompile -u - option
TODO replace GetLastError with SetError/AddCompileError/AddCompileWarning
TODO u16.h u16.cpp include fron folder of kmx_u16.h
TOTO use wchar_t* as cmd-lne-par in main ??
TOTO changes inside VerifyKeyboard()
TOTO changes inside FixupKeyboard()
TODO check if I can use files from some other keyman path instead of a copy in keymap ( e.g. filesystem.h exists elsewhere)
TODO ...

//---------------------------
TOASK is using string OK, or do we use char, wchar?
TOASK a-z, A_Z or more keys? ...
TOASK main/wmain? will we use wchar_t Filenames on Linux?
