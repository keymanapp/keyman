# Sentry 2.3 distributable DLLs

https://github.com/keymanapp/keyman/issues/5166

sentry.dll 0.4.9 is not backwardly compatible with sentry.dll 0.2.3. This means that attempts by older clients to use the newer DLL will typically crash in nasty ways. (Note: DLLs are intended to have stable exported interfaces!)

As we currently host sentry.dll in Keyman Engine rather than giving each app its own version of it, this stops us installing
different versions of Keyman Developer and Keyman for Windows, or 3rd party apps.

My intended solution is twofold:

1. Include the 0.2.3 versions of `sentry.dll` and `sentry.x64.dll` in perpetuity in Keyman Engine. This avoids issues with older versions of an app being broken when a newer version of another app is installed.
2. Rename `sentry.dll` to `sentry.0.4.9.dll` (and `sentry.0.4.9.x64.dll`) and bundle it with each app that uses Sentry (or put it in a separate path if renaming is too hard).

Each process will look for `sentry.<version>.dll` *only* in the folder where its executable is (apart from in development scenarios), to avoid accidentally loading an older version. This means including sentry.dll three times for each app (for a total of 6 dlls):

1. Because Keyman Engine, Keyman for Windows and Keyman Develper all use it and we can't rely on Keyman Engine having the same version as Keyman for Windows (we can only rely on Keyman Engine being the same version or newer).
2. Because we want to be able to update it again in the future: if each app has its own copy, this is safe.
3. And of course, including the old v2.3 sentry.dll in Keyman Engine forever.

