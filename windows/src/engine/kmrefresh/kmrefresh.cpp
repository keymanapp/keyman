/*
  kmrefresh triggers a synchronisation of Windows language data to
  Microsoft's 'cloud'. This process is required to ensure that Windows
  does not stomp over the current language configuration when it
  attempts to synchronise the data back from the cloud, for instance
  shortly after logging into the system.

  This cloud functionality was introduced in Windows 10 1809? It is
  not present in Windows 7, 8.1 or Windows 10 RTM. The process should
  run without issues on all platforms, however, and just be a no-op
  where the endpoint is not available.

  The SyncLanguageDataToCloud API endpoint is currently undocumented,
  so this should be considered experimental. Alternative approaches
  have been considered in corresponding issue #4447, but this is both
  lightweight and respectful of user preferences, so this is the
  approach we've opted to take for now. Plus, it works really well.

  One trick to be aware of is that the sync function fails if run from
  WoW64. So make sure you run the version with the correct bitness.
  I've put this check into the caller app (kmcomapi) rather than in
  this process. kmcomapi does not currently check exit codes but you
  could track them e.g. with procmon if you wanted to verify behaviour.

  See #4447 for additional information.
*/
#define STRICT
#include <Windows.h>

typedef void  (WINAPI * PSYNCLANGUAGEDATATOCLOUD)();

int APIENTRY wWinMain(HINSTANCE hInstance,
  HINSTANCE hPrevInstance,
  PWSTR     lpCmdLine,
  int       nCmdShow)
{
  UNREFERENCED_PARAMETER(hPrevInstance);
  UNREFERENCED_PARAMETER(hInstance);
  UNREFERENCED_PARAMETER(lpCmdLine);
  UNREFERENCED_PARAMETER(nCmdShow);

  int nExitCode = 0;

  if(!SUCCEEDED(CoInitializeEx(NULL, COINIT_APARTMENTTHREADED)))
    return 1;

  HMODULE handle = LoadLibrary(L"coreglobconfig.dll");
  if (handle == NULL) {
    CoUninitialize();
    return 2;
  }

  PSYNCLANGUAGEDATATOCLOUD pSync = (PSYNCLANGUAGEDATATOCLOUD)GetProcAddress(handle, "SyncLanguageDataToCloud");
  if (pSync == NULL) {
    FreeLibrary(handle);
    CoUninitialize();
    return 3;
  }

  pSync();

  // The call to SyncLanguageDataToCloud triggers a worker thread to do
  // the work. As of writing, I am unaware of a way to determine that the
  // worker thread has completed, so the sleep gives plenty of time for
  // it to finish. This process is invisible and silent so the fact that
  // it lives in the background for a few seconds should not be a problem.

  Sleep(10000);

  FreeLibrary(handle);
  CoUninitialize();

  return 0;
}
