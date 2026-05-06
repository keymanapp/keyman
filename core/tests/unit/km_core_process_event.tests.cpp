/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
#include <gtest/gtest.h>
#include "path.hpp"
#include "state.hpp"
#include "kmx/kmx_processevent.h"

#include "emscripten_filesystem.h"
#include "kmnkbd/action_items.hpp"
#include "load_kmx_file.hpp"

using namespace km::core::kmx;
km::core::path test_dir;

km_core_option_item test_env_opts[] = {KM_CORE_OPTIONS_END};

#define KEY_DOWN  TRUE
#define KEY_UP    FALSE

struct TestData {
  const char* test_name;

  const char* keyboard_name;
  km_core_virtual_key vkey;
  uint16_t modifier_state;

  // Initial context
  km_core_cu const* context;

  // Whether keydown handled the event. Only relevant for keyup tests.
  bool keydown_handled;

  // Expected actions for keydown and keyup
  std::initializer_list<km_core_action_item> keydown_actions;
  std::initializer_list<km_core_action_item> keyup_actions;
};

std::string GenerateTestName(const testing::TestParamInfo<TestData>& info) {
  return info.param.test_name;
}

class ProcessEventTests : public testing::TestWithParam<TestData> {
protected:
  km_core_keyboard* keyboard = nullptr;
  km_core_state* state       = nullptr;
  KMX_ProcessEvent process_event;

  void Initialize(TestData const& data) {
    km::core::path kmxfile = km::core::path(test_dir / data.keyboard_name);
    auto blob = km::tests::load_kmx_file(kmxfile.native().c_str());

    EXPECT_EQ(km_core_keyboard_load_from_blob(kmxfile.stem().c_str(), blob.data(), blob.size(), &this->keyboard), KM_CORE_STATUS_OK);
    EXPECT_EQ(km_core_state_create(this->keyboard, test_env_opts, &this->state), KM_CORE_STATUS_OK);
    EXPECT_TRUE(this->process_event.Load(blob.data(), blob.size()));
    if (data.context) {
      EXPECT_EQ(km_core_state_context_set_if_needed(this->state, data.context), KM_CORE_CONTEXT_STATUS_UPDATED);
    }
    ((km::core::state*)this->state)->set_backspace_handled_internally(data.keydown_handled);
  }

  void TearDown() override {
    if (this->state) {
      km_core_state_dispose(this->state);
      this->state = nullptr;
    }
    if (this->keyboard) {
      km_core_keyboard_dispose(this->keyboard);
      this->keyboard = nullptr;
    }
  }
};

void print_all_action_items(km_core_state const* state) {
  size_t n = 0;
  auto act = km_core_state_action_items(state, &n);
  std::cout << "Action items:" << std::endl;
  for (size_t i = 0; i < n; i++) {
    print_action_item("", *act++);
  }
  std::cout << "---------------" << std::endl;
}

TEST_P(ProcessEventTests, ReturnsExpectedActionsForKeyDown) {
  auto data = GetParam();
  Initialize(data);
  EXPECT_EQ(km_core_process_event(this->state, data.vkey, data.modifier_state,
    KEY_DOWN, KM_CORE_EVENT_FLAG_DEFAULT), KM_CORE_STATUS_OK);
  // print_all_action_items(this->state);
  EXPECT_TRUE(action_items(this->state, data.keydown_actions));
}

TEST_P(ProcessEventTests, ReturnsExpectedActionsForKeyUp) {
  auto data = GetParam();
  Initialize(data);
  EXPECT_EQ(km_core_process_event(this->state, data.vkey, data.modifier_state, KEY_UP, KM_CORE_EVENT_FLAG_DEFAULT), KM_CORE_STATUS_OK);
  // print_all_action_items(this->state);
  EXPECT_TRUE(action_items(this->state, data.keyup_actions));
}

union backspace_union {
  km_core_backspace_item backspace;
  uint32_t value;
};
// const backspace_union backspace_with_context = {{KM_CORE_BT_CHAR, 'x'}};
const backspace_union backspace_no_context   = {{KM_CORE_BT_CHAR, 0}};

const TestData values[] = {
  //--------------------------------------------------------------------
  // KMN
  // Key with rule
  {"KMN_VKey_A", "kmx/k_005___nul_with_initial_context.kmx", KM_CORE_VKEY_A, 0, u"x", true,
    { // KeyDown
      {KM_CORE_IT_CHAR, { 0, }, {'d'}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  {"KMN_Ctrl_VKey_A", "kmx/k_005___nul_with_initial_context.kmx", KM_CORE_VKEY_A, KM_CORE_MODIFIER_LCTRL, u"x", true,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Key without rule
  {"KMN_VKey_X", "kmx/k_005___nul_with_initial_context.kmx", KM_CORE_VKEY_X, 0, u"x", true,
    { // KeyDown
      {KM_CORE_IT_CHAR, { 0, }, {'x'}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  {"KMN_Ctrl_VKey_X", "kmx/k_005___nul_with_initial_context.kmx", KM_CORE_VKEY_X, KM_CORE_MODIFIER_LCTRL, u"x", true,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Frame key without rule
  {"KMN_VKey_Enter", "kmx/k_000___null_keyboard.kmx", KM_CORE_VKEY_ENTER, 0, u"x", false,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // // Backspace (with available context)
  // // TODO: fix the implementation to make this test pass. Currently failing in KeyUp
  // {"KMN_VKey_Backspace_Ctxt", "kmx/k_000___null_keyboard.kmx", KM_CORE_VKEY_BKSP, 0, u"x", true,
  //   { // KeyDown
  //     // Once we use C++ 20 we can use:
  //     //{KM_CORE_IT_BACK, { 0, }, {.backspace = {KM_CORE_BT_CHAR, 'x'}}},
  //     {KM_CORE_IT_BACK, { 0, }, {backspace_with_context.value}},
  //     {KM_CORE_IT_END}
  //   },
  //   { // KeyUp
  //     {KM_CORE_IT_END}
  //   }
  // },
  // Backspace (without context)
  {"KMN_VKey_Backspace_NoCtxt", "kmx/k_000___null_keyboard.kmx", KM_CORE_VKEY_BKSP, 0, NULL, false,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Ctrl+Backspace (with context)
  {"KMN_Ctrl_VKey_Backspace_Ctxt", "kmx/k_000___null_keyboard.kmx", KM_CORE_VKEY_BKSP, KM_CORE_MODIFIER_LCTRL, u"x", true,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Ctrl+Backspace (no context)
  {"KMN_Ctrl_VKey_Backspace_NoCtxt", "kmx/k_000___null_keyboard.kmx", KM_CORE_VKEY_BKSP, KM_CORE_MODIFIER_LCTRL, NULL, false,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Modifier frame key
  {"KMN_VKey_Shift", "kmx/k_000___null_keyboard.kmx", KM_CORE_VKEY_SHIFT, 0, u"x", false,
    { // KeyDown
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_END}
    }
  },

  //--------------------------------------------------------------------
  // LDML
  // Key with rule
  {"LDML_VKey_A", "ldml/keyboards/k_020_fr.kmx", KM_CORE_VKEY_A, 0, u"x", true,
    { // KeyDown
      {KM_CORE_IT_CHAR, { 0, }, {'q'}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_END}
    }
  },
  {"LDML_Ctrl_VKey_A", "ldml/keyboards/k_020_fr.kmx", KM_CORE_VKEY_A, KM_CORE_MODIFIER_LCTRL, u"x", true,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Key without rule
  {"LDML_VKey_X", "ldml/keyboards/k_000_minimal_keyboard.kmx", KM_CORE_VKEY_X, 0, u"x", true,
    { // KeyDown
      {KM_CORE_IT_END} // LDML: no output without rule
    },
    { // KeyUp
      {KM_CORE_IT_END}
    }
  },
  {"LDML_Ctrl_VKey_X", "ldml/keyboards/k_000_minimal_keyboard.kmx", KM_CORE_VKEY_X, KM_CORE_MODIFIER_LCTRL, u"x", true,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END} // LDML: no output without rule
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Frame key without rule
  {"LDML_VKey_Enter", "ldml/keyboards/k_000_minimal_keyboard.kmx", KM_CORE_VKEY_ENTER, 0, u"x", false,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Backspace (with available context)
  {"LDML_VKey_Backspace_Ctxt", "ldml/keyboards/k_000_minimal_keyboard.kmx", KM_CORE_VKEY_BKSP, 0, u"x", true,
    { // KeyDown
      // Once we use C++ 20 we can use:
      //{KM_CORE_IT_BACK, { 0, }, {.backspace = {KM_CORE_BT_CHAR, 0}}},
      {KM_CORE_IT_BACK, { 0, }, {backspace_no_context.value}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_END}
    }
  },
  // Backspace (without context)
  {"LDML_VKey_Backspace_NoCtxt", "ldml/keyboards/k_000_minimal_keyboard.kmx", KM_CORE_VKEY_BKSP, 0, NULL, false,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Ctrl+Backspace (with context)
  {"LDML_Ctrl_VKey_Backspace_Ctxt", "ldml/keyboards/k_000_minimal_keyboard.kmx", KM_CORE_VKEY_BKSP, KM_CORE_MODIFIER_LCTRL, u"x", false,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Ctrl+Backspace (no context)
  {"LDML_Ctrl_VKey_Backspace_NoCtxt", "ldml/keyboards/k_000_minimal_keyboard.kmx", KM_CORE_VKEY_BKSP, KM_CORE_MODIFIER_LCTRL, NULL, false,
    { // KeyDown
      {KM_CORE_IT_INVALIDATE_CONTEXT, { 0, }, {0}},
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
  // Modifier frame key
  {"LDML_VKey_Shift", "ldml/keyboards/k_000_minimal_keyboard.kmx", KM_CORE_VKEY_SHIFT, 0, u"x", false,
    { // KeyDown
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    },
    { // KeyUp
      {KM_CORE_IT_EMIT_KEYSTROKE, { 0, }, {0}},
      {KM_CORE_IT_END}
    }
  },
};

INSTANTIATE_TEST_SUITE_P(KMXProcessEvent, ProcessEventTests, testing::ValuesIn(values), GenerateTestName);

// provide our own `main` so that we can get the path of the exe so that
// we have a well-defined location to find our test keyboards
int main(int argc, char** argv) {
#ifdef __EMSCRIPTEN__
  test_dir = get_wasm_file_path(km::core::path(argv[0]).parent());
#else
  test_dir = km::core::path(argv[0]).parent();
#endif
  std::cout << "test_dir=" << test_dir.c_str() << std::endl;
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
