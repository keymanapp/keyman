#include <glib.h>

typedef struct {
} TestObjectFixture;

// The purpose of this unit "test" is to have a test so that generating code
// coverage report works with Ubuntu 24.04+.
static void test_stub(TestObjectFixture* fixture, gconstpointer user_data) {
  g_assert_true(TRUE);
}

int main(int argc, char* argv[]) {
  g_test_init(&argc, &argv, NULL);

  g_test_add("/test/test_stub", TestObjectFixture, NULL, NULL, test_stub, NULL);

  return g_test_run();
}
