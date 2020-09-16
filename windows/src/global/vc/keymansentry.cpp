
#include <Windows.h>
#include <keymansentry.h>
#include <keymanversion.h>
#include <sentry.h>
#include <registry.h>
#include <stdio.h>
#include <shlobj.h>
#include <KnownFolders.h>

#define SENTRY_DSN_DESKTOP   "https://92eb58e6005d47daa33c9c9e39458eb7@sentry.keyman.com/5"
#define SENTRY_DSN_DEVELOPER "https://39b25a09410349a58fe12aaf721565af@sentry.keyman.com/6"
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

  char szPath[MAX_PATH + 17]; // length(keyman-sentry-db)+1

  LPITEMIDLIST pidl;
  if (SUCCEEDED(SHGetFolderLocation(0, CSIDL_LOCAL_APPDATA, NULL, 0, &pidl))) {
    if (SUCCEEDED(SHGetPathFromIDListA(pidl, szPath))) {
      char *p = strchr(szPath, 0);
      if (p > szPath && *(p - 1) != '\\') {
        *p++ = '\\';
        *p = 0;
      }

      strcat_s(szPath, "sentry-db");
      sentry_options_set_database_path(options, szPath);
    }
    ILFree(pidl);
  }

  sentry_options_set_release(options, "release-" KEYMAN_VersionWithTag); // matches git tag
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

  char message[64];
  wsprintfA(message, "Exception %x at %p", ExceptionCode, ExceptionAddress);

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

    sentry_capture_event(event);
  }

  fputs(message, stderr);
  fputs("\n", stderr);
  if(g_report_exceptions) {
    fputs("This error has been automatically reported to the Keyman team.\n", stderr);
  }
}

void keyman_sentry_report_message(keyman_sentry_level_t level, const char *message, bool includeStack) {
  const int FRAMES_TO_SKIP = 0;

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

    sentry_capture_event(event);
  }
}

/* Wrappers for main, wmain */

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
  char buf[256];
  sprintf_s(buf, "Started %s", g_logger);
  keyman_sentry_report_message(KEYMAN_SENTRY_LEVEL_INFO, buf);
}

int keyman_sentry_main(bool is_keyman_developer, const char *logger, int argc, char *argv[], int (*run)(int, char**)) {
  keyman_sentry_init(is_keyman_developer, logger);
  keyman_sentry_setexceptionfilter();
  keyman_sentry_report_start();

  if (argc > 1 && !strcmp(argv[1], "-sentry-client-test-exception")) {
    // Undocumented test parameter
    keyman_sentry_test_crash();
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

  int res = run(argc, argv);

  keyman_sentry_shutdown();

  return res;
}

void keyman_sentry_test_crash() {
  fputs("Testing exception reporting:\n", stderr);
  RaiseException(0x0EA0BEEF, EXCEPTION_NONCONTINUABLE, 0, NULL);
}
