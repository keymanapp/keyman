#include "ibusimcontext.h"
#include <linux/input-event-codes.h>
#include <glib-object.h>
#include <glib.h>
#include <ibus.h>
#include "testmodule.h"

static GType test_module_get_type(void);

#define TEST_TYPE_MODULE (test_module_get_type())

static gboolean
test_module_load(GTypeModule *module) {
  TestModule *test_module = TEST_MODULE(module);

  test_module->register_func(module);

  return TRUE;
}

static void
test_module_unload(GTypeModule *module) {
}

static void
test_module_class_init(TestModuleClass *class) {
  GTypeModuleClass *module_class = G_TYPE_MODULE_CLASS(class);

  module_class->load   = test_module_load;
  module_class->unload = test_module_unload;
}

static GType
test_module_get_type(void) {
  static GType object_type = 0;

  if (!object_type) {
    static const GTypeInfo object_info = {
        sizeof(TestModuleClass),  (GBaseInitFunc)NULL,
        (GBaseFinalizeFunc)NULL,  (GClassInitFunc)test_module_class_init,
        (GClassFinalizeFunc)NULL, NULL,
        sizeof(TestModule),       0,
        (GInstanceInitFunc)NULL,  NULL,
    };
    object_type = g_type_register_static(G_TYPE_TYPE_MODULE, "TestModule", &object_info, 0);
  }
  return object_type;
}

GTypeModule *
test_module_new(TestModuleRegisterFunc register_func) {
  TestModule *test_module = g_object_new(TEST_TYPE_MODULE, NULL);
  GTypeModule *module     = G_TYPE_MODULE(test_module);

  test_module->register_func = register_func;

  /* Register the types initially */
  g_type_module_set_name(module, "FOO");
  g_type_module_use(module);
  //g_type_module_unuse(module);

  return G_TYPE_MODULE(module);
}

void
test_module_unuse(GTypeModule *test_module) {
  g_type_module_unuse(test_module);
}
