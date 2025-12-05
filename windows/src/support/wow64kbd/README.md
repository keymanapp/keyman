# wow64kbd support app for researching WOW64 reqs

This app reports on WOW64 identification function results, and reports
on kbdxx.dll file alignment by printing a hex dump of the first 44 bytes
(which is the 32 bit size of the structure). Manual review of the hex
dump should be possible to determine if we have the expected alignment,
which should be 32 bit on a 32 bit Windows installation, but 64 bit on
any 64 bit other Windows installations, even with a 32 bit process.

wow64kbd is not included in the normal build. Build when needed; you can
copy entire folder to a target computer and run `run.bat` to get a full
report.

`./build.sh test` will also run the same test on the local machine.
