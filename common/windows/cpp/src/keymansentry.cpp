
#include <Windows.h>
#include "../include/keymansentry.h"
#include "../include/keymanversion.h"
#include <sentry.h>
#include "../include/registry.h"
#include <stdio.h>
#include <shlobj.h>
#include <string.h>
#include <KnownFolders.h>

#define SENTRY_DSN_DESKTOP   "https://92eb58e6005d47daa33c9c9e39458eb7@o1005580.ingest.sentry.io/5983518"
#define SENTRY_DSN_DEVELOPER "https://39b25a09410349a58fe12aaf721565af@o1005580.ingest.sentry.io/5983519"
//#define SENTRY_DSN_DEVELOPER "https://7b1ff1dae2c8495b84f90dadcf512b84@sentry.io/4853461" // testing only

bool g_report_exceptions, g_report_messages;
char *g_logger;

int keyman_sentry_init(bool is_keyman_developer, const char *logger) {
  sentry_options_t *options = sentry_options_new();
  const char *key;

  g_logger = _strdup(logger);

  if (is_keyman_developer) {
    sentry_options_set_dsn(options, SENTRY_DSN_DEVELOPER);
    key = REGSZ_IDEOptions_CU;
  } else {
    sentry_options_set_dsn(options, SENTRY_DSN_DESKTOP);
    key = REGSZ_KeymanCU;
  }

  // Set the sentry-db-directory to a writeable location

  char szPath[MAX_PATH + 64]; // sufficient length for sentry-0.6.0-db etc

  LPITEMIDLIST pidl;
  if (SUCCEEDED(SHGetFolderLocation(0, CSIDL_LOCAL_APPDATA, NULL, 0, &pidl))) {
    if (SUCCEEDED(SHGetPathFromIDListA(pidl, szPath))) {
      char *p = strchr(szPath, 0);
      if (p > szPath && *(p - 1) != '\\') {
        *p++ = '\\';
        *p = 0;
      }

      strcat_s(szPath, "sentry-" SENTRY_SDK_VERSION "-db");
      sentry_options_set_database_path(options, szPath);
    }
    ILFree(pidl);
  }

  sentry_options_set_release(options, KEYMAN_VersionGitTag); // matches git tag
  sentry_options_set_environment(options, KEYMAN_Environment); // stable, beta, alpha, test, local

  // We don't currently need to set this, because it will be same path
  // as all our c++ executables.
  //sentry_options_set_handler_path(options, "path/to/crashpad_handler");

  HKEY hkey;
  if (RegOpenKeyExA(HKEY_CURRENT_USER, key, 0, KEY_READ, &hkey) == ERROR_SUCCESS) {
    DWORD dwType, dwValue, dwValueSize = 4;

    g_report_exceptions =
      RegQueryValueExA(hkey, REGSZ_AutomaticallyReportErrors, NULL, &dwType, (LPBYTE)&dwValue, &dwValueSize) != ERROR_SUCCESS || dwType != REG_DWORD || dwValue != 0;

    dwValueSize = 4;
    g_report_messages =
      RegQueryValueExA(hkey, REGSZ_AutomaticallyReportUsage, NULL, &dwType, (LPBYTE)&dwValue, &dwValueSize) != ERROR_SUCCESS || dwType != REG_DWORD || dwValue != 0;

    RegCloseKey(hkey);
  }
  else {
    g_report_exceptions = true;
    g_report_messages = true;
  }

  //sentry_options_set_dist(options, NULL);
  //sentry_options_set_debug(options, 1);

  return sentry_init(options);
}

void keyman_sentry_shutdown() {
  sentry_shutdown();
  free(g_logger);
  g_logger = NULL;
}

//
// Capture a stack trace and include the offending crash address at the top of
// the trace. Apart from skipping frames and the inclusion of TopAddr, this is
// very similar to sentry_event_value_add_stacktrace.
//
sentry_value_t CaptureStackTrace(PVOID TopAddr, DWORD FramesToSkip) {
  PVOID walked_backtrace[256];

  WORD frameCount = RtlCaptureStackBackTrace(FramesToSkip, 256, walked_backtrace, NULL);
  if (frameCount == 0) {
    return { 0 };
  }

  sentry_value_t frames = sentry_value_new_list();

  for (int i = (int)frameCount - 1; i >= 0; i--) {
    sentry_value_t frame = sentry_value_new_object();
    char buf[24];
    wsprintfA(buf, "0x%Ix", DWORD_PTR(walked_backtrace[i]));
    sentry_value_set_by_key(frame, "instruction_addr", sentry_value_new_string(buf));
    sentry_value_append(frames, frame);
  }

  // Insert the except address at the top of the stack
  if (TopAddr != NULL) {
    sentry_value_t frame = sentry_value_new_object();
    char buf[24];
    wsprintfA(buf, "0x%Ix", DWORD_PTR(TopAddr));
    sentry_value_set_by_key(frame, "instruction_addr", sentry_value_new_string(buf));
    sentry_value_append(frames, frame);
  }

  sentry_value_t stacktrace = sentry_value_new_object();
  sentry_value_set_by_key(stacktrace, "frames", frames);

  sentry_value_t threads = sentry_value_new_list();
  sentry_value_t thread = sentry_value_new_object();
  sentry_value_set_by_key(thread, "stacktrace", stacktrace);
  sentry_value_append(threads, thread);

  return threads;
}

void keyman_sentry_report_exception(DWORD ExceptionCode, PVOID ExceptionAddress) {
  sentry_value_t event;
  const int FRAMES_TO_SKIP = 0;
  sentry_uuid_t uuid = { 0 };
  char message[64];
  wsprintfA(message, "Exception %x at %p", (unsigned int) ExceptionCode, ExceptionAddress);

  if (g_report_exceptions) {
    event = sentry_value_new_event();

    /*
    When we set exception information, the report is corrupted. Not sure why. So
    for now we won't create as an exception event. We still get all the information
    we want from this.

    Investigating this further at https://forum.sentry.io/t/corrupted-display-when-exception-data-is-set-using-native-sdk/9167/2

    sentry_value_t exc = sentry_value_new_object();
    sentry_value_set_by_key(exc, "type", sentry_value_new_string("Exception"));
    sentry_value_set_by_key(exc, "value", sentry_value_new_string(message));
    sentry_value_set_by_key(event, "exception", exc);*/
    sentry_value_set_by_key(event, "message", sentry_value_new_string(message));
    sentry_value_set_by_key(event, "logger", sentry_value_new_string(g_logger));

    //sentry_event_value_add_stacktrace(event, &ExceptionAddress, 64);
    sentry_value_t threads = CaptureStackTrace(ExceptionAddress, FRAMES_TO_SKIP);
    if (threads._bits != 0) {
      sentry_value_set_by_key(event, "threads", threads);
    }

    uuid = sentry_capture_event(event);
  }

  fputs(message, stderr);
  fputs("\n", stderr);
  if(g_report_exceptions) {
    fputs("This error has been automatically reported to the Keyman team.\n", stderr);
    char uuid_string[37];
    sentry_uuid_as_string(&uuid, uuid_string);
    fprintf(stderr, "Sentry uuid: %s\n", uuid_string);
  }
}

sentry_uuid_t keyman_sentry_report_message(keyman_sentry_level_t level, const char *message, bool includeStack) {
  const int FRAMES_TO_SKIP = 0;
  sentry_uuid_t uuid = { 0 };

  if ((g_report_exceptions && (level == SENTRY_LEVEL_ERROR || level == SENTRY_LEVEL_DEBUG)) || g_report_messages) {
    sentry_value_t event;

    event = sentry_value_new_event();

    event = sentry_value_new_message_event(
      /*level  */ sentry_level_t(level),
      /*logger */ g_logger,
      /*message*/ message
    );

    if (includeStack) {
      sentry_value_t threads = CaptureStackTrace(NULL, FRAMES_TO_SKIP);
      if (threads._bits != 0) {
        sentry_value_set_by_key(event, "threads", threads);
      }
    }

    uuid = sentry_capture_event(event);
  }

  return uuid;
}

/* Wrappers for main, wmain */

#define SENTRY_USE_LOCAL_FILTER
#ifdef SENTRY_USE_LOCAL_FILTER
LPTOP_LEVEL_EXCEPTION_FILTER LastFilter;

LONG WINAPI FilterExceptions(_In_ struct _EXCEPTION_POINTERS *ExceptionInfo) {
  keyman_sentry_report_exception(ExceptionInfo->ExceptionRecord->ExceptionCode, ExceptionInfo->ExceptionRecord->ExceptionAddress);
  keyman_sentry_shutdown();
  return EXCEPTION_EXECUTE_HANDLER;
}

void keyman_sentry_setexceptionfilter() {
  LastFilter = SetUnhandledExceptionFilter(FilterExceptions);
}
#else
void keyman_sentry_setexceptionfilter() {
}
#endif

void keyman_sentry_report_start() {
  // We used the 'Started' event when testing Sentry integration in 14.0 alpha
  // but we don't want or need it for stable.
  // char buf[256];
  // sprintf_s(buf, "Started %s", g_logger);
  // keyman_sentry_report_message(KEYMAN_SENTRY_LEVEL_INFO, buf);
}

int keyman_sentry_main(bool is_keyman_developer, const char *logger, int argc, char *argv[], int (*run)(int, char**)) {
  if (GetProcAddress(GetModuleHandleW(L"ntdll.dll"), "wine_get_version") != NULL) {
    // We always disable Sentry when running under WINE, because
    // Sentry/dbghelp calls are failing on WINE.
    return run(argc, argv);
  }
  keyman_sentry_init(is_keyman_developer, logger);
  keyman_sentry_setexceptionfilter();
  keyman_sentry_report_start();

  if (argc > 1 && !strcmp(argv[1], "-sentry-client-test-exception")) {
    // Undocumented test parameter
    keyman_sentry_test_crash();
  }

  if (argc > 1 && !strcmp(argv[1], "-sentry-client-test-message")) {
    // Undocumented test parameter
    keyman_sentry_test_message();
  }

  int res = run(argc, argv);

  keyman_sentry_shutdown();

  return res;
}

int keyman_sentry_wmain(bool is_keyman_developer, const char *logger, int argc, wchar_t *argv[], int(*run)(int, wchar_t**)) {
  keyman_sentry_init(is_keyman_developer, logger);
  keyman_sentry_setexceptionfilter();
  keyman_sentry_report_start();

  if (argc > 1 && !wcscmp(argv[1], L"-sentry-client-test-exception")) {
    // Undocumented test parameter
    keyman_sentry_test_crash();
  }

  if (argc > 1 && !wcscmp(argv[1], L"-sentry-client-test-message")) {
    // Undocumented test parameter
    keyman_sentry_test_message();
  }

  int res = run(argc, argv);

  keyman_sentry_shutdown();

  return res;
}

void keyman_sentry_test_message() {
  fputs("Testing Sentry reporting:\n", stderr);
  auto uuid = keyman_sentry_report_message(KEYMAN_SENTRY_LEVEL_INFO, "Testing Sentry message reporting", true);
  char uuid_string[37];
  sentry_uuid_as_string(&uuid, uuid_string);
  fprintf(stderr, "Sentry uuid: %s\n", uuid_string);
  exit(0);
}
void keyman_sentry_test_crash() {
  fputs("Testing Sentry exception reporting:\n", stderr);
  RaiseException(0x0EA0BEEF, EXCEPTION_NONCONTINUABLE, 0, NULL);
  fputs("Should not have gotten here\n", stderr);
  exit(1);
}

/* Delay load sentry.dll from our subdirectory: #5166 */
#include <delayimp.h>

#define SENTRY_BASE_DLL "sentry.dll"
#ifdef _WIN64
#define SENTRY_DLL "sentry.x64.dll"
#else
#define SENTRY_DLL SENTRY_BASE_DLL
#endif
#define SENTRY_INSTALL_PATH "sentry-" SENTRY_SDK_VERSION "\\" SENTRY_DLL
#define SENTRY_DEV_PATH "common\\windows\\delphi\\ext\\sentry\\" SENTRY_DLL
#define ENV_KEYMAN_ROOT "KEYMAN_ROOT"

HMODULE LoadSentryLibrary() {
  //MAX_PATH + 64 chars leaves space for "\sentry-0.6.0\sentry.x64.dll"
  char buf[MAX_PATH + 64], keyman_root[MAX_PATH + 64];

  int nsize = GetModuleFileNameA(0, buf, MAX_PATH);
  if (nsize == 0 || nsize == MAX_PATH)
    return NULL;

  char drive[_MAX_DRIVE], dir[_MAX_DIR + 64], name[_MAX_FNAME], ext[_MAX_EXT];
  _splitpath_s(buf, drive, dir, name, ext);

  nsize = GetEnvironmentVariableA(ENV_KEYMAN_ROOT, keyman_root, MAX_PATH);
  if (nsize > 0 && nsize < MAX_PATH) {
    // We are potentially running in keyman source tree
    _makepath_s(buf, drive, dir, NULL, NULL);
    if (_strnicmp(buf, keyman_root, strlen(keyman_root)) == 0) {
      if (*(strchr(keyman_root, 0) - 1) != '\\') strcat_s(keyman_root, "\\");
      strcat_s(keyman_root, SENTRY_DEV_PATH);
      HMODULE result = LoadLibraryA(keyman_root);
      if (result) return result;
    }
  }

  _makepath_s(buf, drive, dir, NULL, NULL);
  strcat_s(buf, SENTRY_INSTALL_PATH);
  return LoadLibraryA(buf);
}

FARPROC WINAPI delayHook(unsigned dliNotify, PDelayLoadInfo pdli)
{
  switch (dliNotify) {
  case dliStartProcessing:

    // If you want to return control to the helper, return 0.
    // Otherwise, return a pointer to a FARPROC helper function
    // that will be used instead, thereby bypassing the rest
    // of the helper.

    break;

  case dliNotePreLoadLibrary:

    // If you want to return control to the helper, return 0.
    // Otherwise, return your own HMODULE to be used by the
    // helper instead of having it call LoadLibrary itself.
    if (_stricmp(pdli->szDll, SENTRY_BASE_DLL) == 0) {
      return reinterpret_cast<FARPROC>(LoadSentryLibrary());
    }

    break;

  case dliNotePreGetProcAddress:

    // If you want to return control to the helper, return 0.
    // If you choose you may supply your own FARPROC function
    // address and bypass the helper's call to GetProcAddress.

    break;

  case dliFailLoadLib:

    // LoadLibrary failed.
    // If you don't want to handle this failure yourself, return 0.
    // In this case the helper will raise an exception
    // (ERROR_MOD_NOT_FOUND) and exit.
    // If you want to handle the failure by loading an alternate
    // DLL (for example), then return the HMODULE for
    // the alternate DLL. The helper will continue execution with
    // this alternate DLL and attempt to find the
    // requested entrypoint via GetProcAddress.

    break;

  case dliFailGetProc:

    // GetProcAddress failed.
    // If you don't want to handle this failure yourself, return 0.
    // In this case the helper will raise an exception
    // (ERROR_PROC_NOT_FOUND) and exit.
    // If you choose, you may handle the failure by returning
    // an alternate FARPROC function address.

    break;

  case dliNoteEndProcessing:

    // This notification is called after all processing is done.
    // There is no opportunity for modifying the helper's behavior
    // at this point except by longjmp()/throw()/RaiseException.
    // No return value is processed.

    break;

  default:

    return NULL;
  }

  return NULL;
}

ExternC const PfnDliHook __pfnDliNotifyHook2 = delayHook;
ExternC const PfnDliHook __pfnDliFailureHook2 = delayHook;
