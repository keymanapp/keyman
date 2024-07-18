#ifndef __TESTMODULE_H__
#define __TESTMODULE_H__

G_BEGIN_DECLS

typedef struct _TestModule TestModule;
typedef struct _TestModuleClass TestModuleClass;

#define TEST_MODULE(module) (G_TYPE_CHECK_INSTANCE_CAST((module), TEST_TYPE_MODULE, TestModule))
#define TEST_MODULE_CLASS(class) (G_TYPE_CHECK_CLASS_CAST((class), TEST_TYPE_MODULE, TestModuleClass))
#define TEST_IS_MODULE(module) (G_TYPE_CHECK_INSTANCE_TYPE((module), TEST_TYPE_MODULE))
#define TEST_IS_MODULE_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE((class), TEST_TYPE_MODULE))
#define TEST_MODULE_GET_CLASS(module) (G_TYPE_INSTANCE_GET_CLASS((module), TEST_TYPE_MODULE, TestModuleClass))
typedef void (*TestModuleRegisterFunc)(GTypeModule* module);

struct _TestModule {
  GTypeModule parent_instance;

  TestModuleRegisterFunc register_func;
};

struct _TestModuleClass {
  GTypeModuleClass parent_class;
};

GTypeModule* test_module_new(TestModuleRegisterFunc register_func);
void test_module_unuse(GTypeModule* test_module);

G_END_DECLS

#endif // __TESTMODULE_H__
