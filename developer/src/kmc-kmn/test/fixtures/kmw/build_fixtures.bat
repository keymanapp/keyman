@echo off
echo Compiles the keyboards using the legacy kmcomp.exe
echo to use as baseline comparisons for kmc kmw module
rem we run for the .kmx first even though we don't use them because the legacy
rem cmdline web compiler has a bit of an issue generating the VK properly if we
rem build the .kmn directly and haven't already built the .kmx (which generates
rem the .kvk that the web compiler is expecting). This doesn't happen in project
rem mode because the .kmx is always built first. Probably won't fix this because
rem legacy compiler is being replaced anyway.
for %%d in (*.kmn) do ..\..\..\..\..\bin\kmcomp -no-compiler-version -d %%d %%~nd.kmx
for %%d in (*.kmn) do ..\..\..\..\..\bin\kmcomp -no-compiler-version -d %%d %%~nd.js
rem ok, we no longer need .kmx and .kvk once we've built the .js fixtures
del *.kmx
rem We can't do `del *.kvk` because that also deletes *.kvks, thanks 8.3
rem filenames! But the following horror works:
for %%d in (*.kvk) do if %%~xd == .kvk del %%d

