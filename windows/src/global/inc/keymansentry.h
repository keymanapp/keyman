#pragma once

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

int keyman_sentry_init(bool is_keyman_developer);
void keyman_sentry_shutdown();

// recommended wrapper function - sets up exception handling, etc
int keyman_sentry_main(bool is_keyman_developer, int argc, char *argv[], int(*run)(int, char**));
int keyman_sentry_wmain(bool is_keyman_developer, int argc, wchar_t *argv[], int(*run)(int, wchar_t**));

void keyman_sentry_report_message(keyman_sentry_level_t level, const char *logger, const char *message, bool includeStack = false);
