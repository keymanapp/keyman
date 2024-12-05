#include <glib-object.h>
#include <glib.h>
#include <gtk/gtk.h>
#include "bcp47util.h"

typedef struct {
} Bcp47UtilFixture;

typedef struct {
  const char *tag;
  const char *expected;
  int expectedResult;
} TestData;

static void
test_bcp47_minimize(Bcp47UtilFixture *fixture, gconstpointer user_data) {
  TestData *testData = (TestData *)user_data;
  int capacity        = 255;
  char minimizedTag[capacity];
  int result = bcp47_minimize(testData->tag, minimizedTag, capacity);
  g_assert_cmpint(result, ==, testData->expectedResult);
  g_assert_cmpstr(minimizedTag, ==, testData->expected);
}

static void
test_bcp47_get_language_code(Bcp47UtilFixture *fixture, gconstpointer user_data) {
  TestData *testData = (TestData *)user_data;
  int capacity       = 255;
  char lang_code[capacity];
  int result = bcp47_get_language_code(testData->tag, lang_code, capacity);
  g_assert_cmpint(result, ==, testData->expectedResult);
  g_assert_cmpstr(lang_code, ==, testData->expected);
}

int
main(int argc, char *argv[]) {
  gtk_init(&argc, &argv);
  g_test_init(&argc, &argv, NULL);
  g_test_set_nonfatal_assertions();

  TestData testData1 = {NULL, "", -1};
  g_test_add("/bcp47util/minimize/NULL", Bcp47UtilFixture, &testData1, NULL, test_bcp47_minimize, NULL);

  TestData testData2 = {"", "", -1};
  g_test_add("/bcp47util/minimize/EmptyString", Bcp47UtilFixture, &testData2, NULL, test_bcp47_minimize, NULL);

  TestData testData3 = { "fuf", "fuf", 3 };
  g_test_add("/bcp47util/minimize/fuf", Bcp47UtilFixture, &testData3, NULL, test_bcp47_minimize, NULL);

  TestData testData4 = {"fuf-Latn", "fuf", 3};
  g_test_add("/bcp47util/minimize/fuf-Latn", Bcp47UtilFixture, &testData4, NULL, test_bcp47_minimize, NULL);

  TestData testData5 = {"fuf-Arab", "fuf-Arab", 8};
  g_test_add("/bcp47util/minimize/fuf-Arab", Bcp47UtilFixture, &testData5, NULL, test_bcp47_minimize, NULL);

  TestData testData6 = {"fuf-Adlm-ML", "fuf-Adlm-ML", 11};
  g_test_add("/bcp47util/minimize/fuf-Adlm-ML", Bcp47UtilFixture, &testData6, NULL, test_bcp47_minimize, NULL);

  TestData testData7 = {"und", "und", 3};
  g_test_add("/bcp47util/minimize/und", Bcp47UtilFixture, &testData7, NULL, test_bcp47_minimize, NULL);

  TestData testData8 = {"und-Latn", "und-Latn", 8};
  g_test_add("/bcp47util/minimize/und-Latn", Bcp47UtilFixture, &testData8, NULL, test_bcp47_minimize, NULL);

  TestData testData9 = {"und-fonipa", "und-fonipa", 10};
  g_test_add("/bcp47util/minimize/und-fonipa", Bcp47UtilFixture, &testData9, NULL, test_bcp47_minimize, NULL);

  TestData testData10 = {"und-Latn-fonipa", "und-fonipa", 10};
  g_test_add("/bcp47util/minimize/und-Latn-fonipa", Bcp47UtilFixture, &testData10, NULL, test_bcp47_minimize, NULL);

  TestData testData11 = {"mul", "mul", 3};
  g_test_add("/bcp47util/minimize/mul", Bcp47UtilFixture, &testData11, NULL, test_bcp47_minimize, NULL);

  // bcp47_get_language_code tests
  TestData testData21 = {NULL, "", FALSE};
  g_test_add("/bcp47util/langcode/NULL", Bcp47UtilFixture, &testData21, NULL, test_bcp47_get_language_code, NULL);

  TestData testData22 = {"", "", FALSE};
  g_test_add("/bcp47util/langcode/EmptyString", Bcp47UtilFixture, &testData22, NULL, test_bcp47_get_language_code, NULL);

  TestData testData23 = {"fuf", "fuf", TRUE};
  g_test_add("/bcp47util/langcode/fuf", Bcp47UtilFixture, &testData23, NULL, test_bcp47_get_language_code, NULL);

  TestData testData24 = {"fuf-Latn", "fuf", TRUE};
  g_test_add("/bcp47util/langcode/fuf-Latn", Bcp47UtilFixture, &testData24, NULL, test_bcp47_get_language_code, NULL);

  TestData testData25 = {"fuf-Arab", "fuf", TRUE};
  g_test_add("/bcp47util/langcode/fuf-Arab", Bcp47UtilFixture, &testData25, NULL, test_bcp47_get_language_code, NULL);

  TestData testData26 = {"fuf-Adlm-ML", "fuf", TRUE};
  g_test_add("/bcp47util/langcode/fuf-Adlm-ML", Bcp47UtilFixture, &testData26, NULL, test_bcp47_get_language_code, NULL);

  TestData testData27 = {"und", "und", TRUE};
  g_test_add("/bcp47util/langcode/und", Bcp47UtilFixture, &testData27, NULL, test_bcp47_get_language_code, NULL);

  TestData testData28 = {"und-Latn", "und", TRUE};
  g_test_add("/bcp47util/langcode/und-Latn", Bcp47UtilFixture, &testData28, NULL, test_bcp47_get_language_code, NULL);

  return g_test_run();
}
