/* GLib testing utilities
 * Copyright (C) 2007 Imendio AB
 * Authors: Tim Janik
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __KM_G_TEST_UTILS_H__
#define __KM_G_TEST_UTILS_H__

#include <glib.h>
#include <errno.h>
#include <string.h>

G_BEGIN_DECLS

typedef struct GTestCase  GTestCase;
typedef struct GTestSuite GTestSuite;
typedef void (*GTestFunc)        (void);
typedef void (*GTestDataFunc)    (gconstpointer user_data);
typedef void (*GTestFixtureFunc) (gpointer      fixture,
                                  gconstpointer user_data);

GLIB_AVAILABLE_IN_ALL
int     g_strcmp0                       (const char     *str1,
                                         const char     *str2);

/* report performance results */
GLIB_AVAILABLE_IN_ALL
void    km_g_test_minimized_result         (double          minimized_quantity,
                                         const char     *format,
                                         ...) G_GNUC_PRINTF (2, 3);
GLIB_AVAILABLE_IN_ALL
void    km_g_test_maximized_result         (double          maximized_quantity,
                                         const char     *format,
                                         ...) G_GNUC_PRINTF (2, 3);

/* initialize testing framework */
GLIB_AVAILABLE_IN_ALL
void    km_g_test_init                     (int            *argc,
                                         char         ***argv,
                                         ...) G_GNUC_NULL_TERMINATED;

/**
 * KM_G_TEST_OPTION_ISOLATE_DIRS:
 *
 * Creates a unique temporary directory for each unit test and uses
 * g_set_user_dirs() to set XDG directories to point into subdirectories of it
 * for the duration of the unit test. The directory tree is cleaned up after the
 * test finishes successfully. Note that this doesn’t take effect until
 * km_g_test_run() is called, so calls to (for example) g_get_user_home_dir() will
 * return the system-wide value when made in a test program’s main() function.
 *
 * The following functions will return subdirectories of the temporary directory
 * when this option is used. The specific subdirectory paths in use are not
 * guaranteed to be stable API — always use a getter function to retrieve them.
 *
 *  - g_get_home_dir()
 *  - g_get_user_cache_dir()
 *  - g_get_system_config_dirs()
 *  - g_get_user_config_dir()
 *  - g_get_system_data_dirs()
 *  - g_get_user_data_dir()
 *  - g_get_user_state_dir()
 *  - g_get_user_runtime_dir()
 *
 * The subdirectories may not be created by the test harness; as with normal
 * calls to functions like g_get_user_cache_dir(), the caller must be prepared
 * to create the directory if it doesn’t exist.
 *
 * Since: 2.60
 */
#define KM_G_TEST_OPTION_ISOLATE_DIRS "isolate_dirs"

/* While we discourage its use, g_assert() is often used in unit tests
 * (especially in legacy code). g_assert_*() should really be used instead.
 * g_assert() can be disabled at client program compile time, which can render
 * tests useless. Highlight that to the user. */
#ifdef G_DISABLE_ASSERT
#if defined(G_HAVE_ISO_VARARGS)
#define km_g_test_init(argc, argv, ...) \
  G_STMT_START { \
    g_printerr ("Tests were compiled with G_DISABLE_ASSERT and are likely no-ops. Aborting.\n"); \
    exit (1); \
  } G_STMT_END
#elif defined(G_HAVE_GNUC_VARARGS)
#define km_g_test_init(argc, argv...) \
  G_STMT_START { \
    g_printerr ("Tests were compiled with G_DISABLE_ASSERT and are likely no-ops. Aborting.\n"); \
    exit (1); \
  } G_STMT_END
#else  /* no varargs */
  /* do nothing */
#endif  /* varargs support */
#endif  /* G_DISABLE_ASSERT */

/* query testing framework config */
#define km_g_test_initialized()            (km_g_test_config_vars->test_initialized)
#define km_g_test_quick()                  (km_g_test_config_vars->test_quick)
#define km_g_test_slow()                   (!km_g_test_config_vars->test_quick)
#define km_g_test_thorough()               (!km_g_test_config_vars->test_quick)
#define km_g_test_perf()                   (km_g_test_config_vars->test_perf)
#define km_g_test_verbose()                (km_g_test_config_vars->test_verbose)
#define km_g_test_quiet()                  (km_g_test_config_vars->test_quiet)
#define km_g_test_undefined()              (km_g_test_config_vars->test_undefined)
GLIB_AVAILABLE_IN_2_38
gboolean km_g_test_subprocess (void);

/* run all tests under toplevel suite (path: /) */
GLIB_AVAILABLE_IN_ALL
int     km_g_test_run                      (void);
/* hook up a test functions under test path */
GLIB_AVAILABLE_IN_ALL
void    km_g_test_add_func                 (const char     *testpath,
                                         GTestFunc       test_func);

GLIB_AVAILABLE_IN_ALL
void    km_g_test_add_data_func            (const char     *testpath,
                                         gconstpointer   test_data,
                                         GTestDataFunc   test_func);

GLIB_AVAILABLE_IN_2_34
void    km_g_test_add_data_func_full       (const char     *testpath,
                                         gpointer        test_data,
                                         GTestDataFunc   test_func,
                                         GDestroyNotify  data_free_func);

/* tell about currently run test */
GLIB_AVAILABLE_IN_2_68
const char * km_g_test_get_path            (void);

/* tell about failure */
GLIB_AVAILABLE_IN_2_30
void    km_g_test_fail                     (void);
GLIB_AVAILABLE_IN_2_70
void    km_g_test_fail_printf              (const char *format,
                                         ...) G_GNUC_PRINTF (1, 2);
GLIB_AVAILABLE_IN_2_38
void    km_g_test_incomplete               (const gchar *msg);
GLIB_AVAILABLE_IN_2_70
void    km_g_test_incomplete_printf        (const char *format,
                                         ...) G_GNUC_PRINTF (1, 2);
GLIB_AVAILABLE_IN_2_38
void    km_g_test_skip                     (const gchar *msg);
GLIB_AVAILABLE_IN_2_70
void    km_g_test_skip_printf              (const char *format,
                                         ...) G_GNUC_PRINTF (1, 2);
GLIB_AVAILABLE_IN_2_38
gboolean km_g_test_failed                  (void);
GLIB_AVAILABLE_IN_2_38
void    km_g_test_set_nonfatal_assertions  (void);

/**
 * km_g_test_add:
 * @testpath: the test path for a new test case
 * @Fixture: the type of a fixture data structure
 * @tdata: data argument for the test functions
 * @fsetup: the function to set up the fixture data
 * @ftest: the actual test function
 * @fteardown: the function to tear down the fixture data
 *
 * Hooks up a new test case at @testpath.
 *
 * This function is similar to [func@GLib.test_add_func].
 *
 * A fixture data structure with setup and teardown functions
 * may be provided, similar to [func@GLib.test_create_case].
 *
 * `km_g_test_add()` is implemented as a macro, so that the @fsetup,
 * @ftest and @fteardown callbacks can expect a @Fixture pointer
 * as their first argument in a type safe manner. They otherwise
 * have type `GTestFixtureFunc`.
 *
 * Since: 2.16
 */
#define km_g_test_add(testpath, Fixture, tdata, fsetup, ftest, fteardown) \
					G_STMT_START {			\
                                         void (*add_vtable) (const char*,       \
                                                    gsize,             \
                                                    gconstpointer,     \
                                                    void (*) (Fixture*, gconstpointer),   \
                                                    void (*) (Fixture*, gconstpointer),   \
                                                    void (*) (Fixture*, gconstpointer)) =  (void (*) (const gchar *, gsize, gconstpointer, void (*) (Fixture*, gconstpointer), void (*) (Fixture*, gconstpointer), void (*) (Fixture*, gconstpointer))) km_g_test_add_vtable; \
                                         add_vtable \
                                          (testpath, sizeof (Fixture), tdata, fsetup, ftest, fteardown); \
					} G_STMT_END

/* add test messages to the test report */
GLIB_AVAILABLE_IN_ALL
void    km_g_test_message                  (const char *format,
                                         ...) G_GNUC_PRINTF (1, 2);
GLIB_AVAILABLE_IN_ALL
void    km_g_test_bug_base                 (const char *uri_pattern);
GLIB_AVAILABLE_IN_ALL
void    km_g_test_bug                      (const char *bug_uri_snippet);
GLIB_AVAILABLE_IN_2_62
void    km_g_test_summary                  (const char *summary);
/* measure test timings */
GLIB_AVAILABLE_IN_ALL
void    km_g_test_timer_start              (void);
GLIB_AVAILABLE_IN_ALL
double  km_g_test_timer_elapsed            (void); /* elapsed seconds */
GLIB_AVAILABLE_IN_ALL
double  km_g_test_timer_last               (void); /* repeat last elapsed() result */

/* automatically g_free or g_object_unref upon teardown */
GLIB_AVAILABLE_IN_ALL
void    km_g_test_queue_free               (gpointer gfree_pointer);
GLIB_AVAILABLE_IN_ALL
void    km_g_test_queue_destroy            (GDestroyNotify destroy_func,
                                         gpointer       destroy_data);
#define km_g_test_queue_unref(gobject)     km_g_test_queue_destroy (g_object_unref, gobject)

/**
 * GTestTrapFlags:
 * @G_TEST_TRAP_DEFAULT: Default behaviour. Since: 2.74
 * @G_TEST_TRAP_SILENCE_STDOUT: Redirect stdout of the test child to
 *     `/dev/null` so it cannot be observed on the console during test
 *     runs. The actual output is still captured though to allow later
 *     tests with km_g_test_trap_assert_stdout().
 * @G_TEST_TRAP_SILENCE_STDERR: Redirect stderr of the test child to
 *     `/dev/null` so it cannot be observed on the console during test
 *     runs. The actual output is still captured though to allow later
 *     tests with km_g_test_trap_assert_stderr().
 * @G_TEST_TRAP_INHERIT_STDIN: If this flag is given, stdin of the
 *     child process is shared with stdin of its parent process.
 *     It is redirected to `/dev/null` otherwise.
 *
 * Test traps are guards around forked tests.
 * These flags determine what traps to set.
 *
 * Deprecated: 2.38: #GTestTrapFlags is used only with km_g_test_trap_fork(),
 * which is deprecated. km_g_test_trap_subprocess() uses
 * #GTestSubprocessFlags.
 */
G_GNUC_BEGIN_IGNORE_DEPRECATIONS

GLIB_DEPRECATED_IN_2_38_FOR (km_g_test_trap_subprocess)
gboolean km_g_test_trap_fork               (guint64              usec_timeout,
                                         GTestTrapFlags       test_trap_flags);

G_GNUC_END_IGNORE_DEPRECATIONS

GLIB_AVAILABLE_IN_2_38
void     km_g_test_trap_subprocess         (const char           *test_path,
                                         guint64               usec_timeout,
                                         GTestSubprocessFlags  test_flags);

GLIB_AVAILABLE_IN_ALL
gboolean km_g_test_trap_has_passed(void);
GLIB_AVAILABLE_IN_ALL
gboolean km_g_test_trap_reached_timeout(void);
#define km_g_test_trap_assert_passed() km_g_test_trap_assertions(G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, 0, 0)
#define km_g_test_trap_assert_failed() km_g_test_trap_assertions(G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, 1, 0)
#define km_g_test_trap_assert_stdout(soutpattern) \
  km_g_test_trap_assertions(G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, 2, soutpattern)
#define km_g_test_trap_assert_stdout_unmatched(soutpattern) \
  km_g_test_trap_assertions(G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, 3, soutpattern)
#define km_g_test_trap_assert_stderr(serrpattern) \
  km_g_test_trap_assertions(G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, 4, serrpattern)
#define km_g_test_trap_assert_stderr_unmatched(serrpattern) \
  km_g_test_trap_assertions(G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, 5, serrpattern)

/* provide seed-able random numbers for tests */
#define km_g_test_rand_bit() (0 != (g_test_rand_int() & (1 << 15)))
GLIB_AVAILABLE_IN_ALL
gint32 km_g_test_rand_int(void);
GLIB_AVAILABLE_IN_ALL
gint32 km_g_test_rand_int_range(gint32 begin, gint32 end);
GLIB_AVAILABLE_IN_ALL
double km_g_test_rand_double(void);
GLIB_AVAILABLE_IN_ALL
double km_g_test_rand_double_range(double range_start, double range_end);

/*
 * semi-internal API: non-documented symbols with stable ABI. You
 * should use the non-internal helper macros instead. However, for
 * compatibility reason, you may use this semi-internal API.
 */
GLIB_AVAILABLE_IN_ALL
GTestCase*    km_g_test_create_case        (const char       *test_name,
                                         gsize             data_size,
                                         gconstpointer     test_data,
                                         GTestFixtureFunc  data_setup,
                                         GTestFixtureFunc  data_test,
                                         GTestFixtureFunc  data_teardown);
GLIB_AVAILABLE_IN_ALL
GTestSuite*   km_g_test_create_suite       (const char       *suite_name);
GLIB_AVAILABLE_IN_ALL
GTestSuite*   km_g_test_get_root           (void);
GLIB_AVAILABLE_IN_ALL
void          km_g_test_suite_add          (GTestSuite     *suite,
                                         GTestCase      *test_case);
GLIB_AVAILABLE_IN_ALL
void          km_g_test_suite_add_suite    (GTestSuite     *suite,
                                         GTestSuite     *nestedsuite);
GLIB_AVAILABLE_IN_ALL
int           km_g_test_run_suite          (GTestSuite     *suite);

GLIB_AVAILABLE_IN_2_70
void          km_g_test_case_free          (GTestCase *test_case);

GLIB_AVAILABLE_IN_2_70
void          km_g_test_suite_free         (GTestSuite     *suite);

GLIB_AVAILABLE_IN_ALL
void    km_g_test_trap_assertions          (const char     *domain,
                                         const char     *file,
                                         int             line,
                                         const char     *func,
                                         guint64         assertion_flags, /* 0-pass, 1-fail, 2-outpattern, 4-errpattern */
                                         const char     *pattern);
GLIB_AVAILABLE_IN_ALL
void    g_assertion_message             (const char     *domain,
                                         const char     *file,
                                         int             line,
                                         const char     *func,
                                         const char     *message) G_ANALYZER_NORETURN;
G_NORETURN
GLIB_AVAILABLE_IN_ALL
void    g_assertion_message_expr        (const char     *domain,
                                         const char     *file,
                                         int             line,
                                         const char     *func,
                                         const char     *expr);
GLIB_AVAILABLE_IN_ALL
void    g_assertion_message_cmpstr      (const char     *domain,
                                         const char     *file,
                                         int             line,
                                         const char     *func,
                                         const char     *expr,
                                         const char     *arg1,
                                         const char     *cmp,
                                         const char     *arg2) G_ANALYZER_NORETURN;

GLIB_AVAILABLE_IN_2_68
void    g_assertion_message_cmpstrv     (const char         *domain,
                                         const char         *file,
                                         int                 line,
                                         const char         *func,
                                         const char         *expr,
                                         const char * const *arg1,
                                         const char * const *arg2,
                                         gsize               first_wrong_idx) G_ANALYZER_NORETURN;
GLIB_AVAILABLE_IN_ALL
void    g_assertion_message_cmpnum      (const char     *domain,
                                         const char     *file,
                                         int             line,
                                         const char     *func,
                                         const char     *expr,
                                         long double     arg1,
                                         const char     *cmp,
                                         long double     arg2,
                                         char            numtype) G_ANALYZER_NORETURN;
GLIB_AVAILABLE_IN_ALL
void    g_assertion_message_error       (const char     *domain,
                                         const char     *file,
                                         int             line,
                                         const char     *func,
                                         const char     *expr,
                                         const GError   *error,
                                         GQuark          error_domain,
                                         int             error_code) G_ANALYZER_NORETURN;
GLIB_AVAILABLE_IN_ALL
void    km_g_test_add_vtable               (const char     *testpath,
                                         gsize           data_size,
                                         gconstpointer   test_data,
                                         GTestFixtureFunc  data_setup,
                                         GTestFixtureFunc  data_test,
                                         GTestFixtureFunc  data_teardown);

GLIB_VAR const GTestConfig * const km_g_test_config_vars;

/* internal logging API */
GLIB_AVAILABLE_IN_ALL
const char*     km_g_test_log_type_name    (GTestLogType    log_type);
GLIB_AVAILABLE_IN_ALL
GTestLogBuffer* km_g_test_log_buffer_new   (void);
GLIB_AVAILABLE_IN_ALL
void            km_g_test_log_buffer_free  (GTestLogBuffer *tbuffer);
GLIB_AVAILABLE_IN_ALL
void            km_g_test_log_buffer_push  (GTestLogBuffer *tbuffer,
                                         guint           n_bytes,
                                         const guint8   *bytes);
GLIB_AVAILABLE_IN_ALL
GTestLogMsg*    km_g_test_log_buffer_pop   (GTestLogBuffer *tbuffer);
GLIB_AVAILABLE_IN_ALL
void            km_g_test_log_msg_free     (GTestLogMsg    *tmsg);

GLIB_AVAILABLE_IN_2_34
void    km_g_test_expect_message                    (const gchar    *log_domain,
                                                  GLogLevelFlags  log_level,
                                                  const gchar    *pattern);
GLIB_AVAILABLE_IN_2_34
void    km_g_test_assert_expected_messages_internal (const char     *domain,
                                                  const char     *file,
                                                  int             line,
                                                  const char     *func);

GLIB_AVAILABLE_IN_2_38
gchar * km_g_test_build_filename                    (GTestFileType   file_type,
                                                  const gchar    *first_path,
                                                  ...) G_GNUC_NULL_TERMINATED;
GLIB_AVAILABLE_IN_2_38
const gchar *km_g_test_get_dir                      (GTestFileType   file_type);
GLIB_AVAILABLE_IN_2_38
const gchar *km_g_test_get_filename                 (GTestFileType   file_type,
                                                  const gchar    *first_path,
                                                  ...) G_GNUC_NULL_TERMINATED;

#define km_g_test_assert_expected_messages() km_g_test_assert_expected_messages_internal (G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC)

G_END_DECLS

#endif /* __KM_G_TEST_UTILS_H__ */
