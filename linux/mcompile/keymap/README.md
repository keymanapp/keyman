# Keymap

Sample program that reads US basic keyboard and compares to key value group



TODO check if US basic is the right Keyboard to compare with
TODO non-letter characters don't work OK yet
TODO Umlauts don't work OK yet
TODO Check for use of correct dimensions in Vector/prevent error if dims are not correct
TODO prevent crashes: handle possible Errors in CreateCompleteRow_US, Split_US_To_3D_Vector
TODO check Keycode of TLDE, BKSL, LSGT
TODO remove unnecessary printf/cout
TODO path for xkb/symbols as compile time option in meson
TODO append_other_ToVector: ensure shift states of GetKeyvalsFromKeymap are not out of range
TODO check how many/which shift states we use ( at the moment we read all shiftstate-columns of US but then use only 2 colums  
     (non-shift + shift) then use as many colums for Other )

TODO define folder to store File_US.txt" in and find better name
TODO get rid of GTK functions that are deprecated and use X11 instead
TODO retrieve name of Other keyboard and use appropriate name instead of "Other"
TODO ...
