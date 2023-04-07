#pragma once

#include <sentry.h>

/*
 * Map of Sentry levels for events and breadcrumbs.
 */
typedef enum keyman_sentry_level_e {
  KEYMAN_SENTRY_LEVEL_DEBUG = -1,
  KEYMAN_SENTRY_LEVEL_INFO = 0,
  KEYMAN_SENTRY_LEVEL_WARNING = 1,
  KEYMAN_SENTRY_LEVEL_ERROR = 2,
  KEYMAN_SENTRY_LEVEL_FATAL = 3,
} keyman_sentry_level_t;

#define KEYMAN_SENTRY_LOGGER_DEVELOPER_TOOLS "KeymanDeveloper.Tools"
#define KEYMAN_SENTRY_LOGGER_DEVELOPER_IDE   "KeymanDeveloper.IDE"

#define KEYMAN_SENTRY_LOGGER_DESKTOP         "KeymanWindows"
#define KEYMAN_SENTRY_LOGGER_DESKTOP_ENGINE  "KeymanWindows.Engine"


int keyman_sentry_init(bool is_keyman_developer, const char *logger);
void keyman_sentry_shutdown();

// recommended wrapper function - sets up exception handling, etc
int keyman_sentry_main(bool is_keyman_developer, const char *logger, int argc, char *argv[], int(*run)(int, char**));
int keyman_sentry_wmain(bool is_keyman_developer, const char *logger, int argc, wchar_t *argv[], int(*run)(int, wchar_t**));

void keyman_sentry_setexceptionfilter();
sentry_uuid_t keyman_sentry_report_message(keyman_sentry_level_t level, const char *message, bool includeStack = false);
void keyman_sentry_report_start();

//
// With this function, we throw a test crash event to make sure that:
// a) exception hooking is in place
// b) events are correctly sent through
// c) symbolication is working
// d) privacy options are correctly checked
//
void keyman_sentry_test_crash();
void keyman_sentry_test_message();
