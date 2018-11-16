#include <keyman/keyboardprocessor.h>
#include <iostream>
#include "processor.hpp"
#include "state.hpp"
#include <kmx/kmxtest.h>

extern const struct KMXTest_ChToVKey chToVKey[];

namespace km {
  namespace kbp
  {

    km_kbp_status kmx_processor::validate() const {
      return m_valid ? KM_KBP_STATUS_OK : KM_KBP_STATUS_INVALID_KEYBOARD;
    }

    kmx_processor::kmx_processor(km_kbp_keyboard_attrs const & kb) : abstract_processor(kb) {
      std::filesystem::path p = kb.folder_path;
      p /= kb.id;
      p.replace_extension(".kmx");
      //char *x = wstrtostr((PKMX_WCHAR) kb.id);
      std::cout << p << std::endl;
      m_valid = (bool) kmx.Load(p.native().c_str());
      //load_keyboard();
    }
    void kmx_processor::load_keyboard() {

    }

    char VKeyToChar(KMX_UINT modifiers, KMX_UINT vk) {
      // We only map SHIFT and UNSHIFTED
      // TODO: Map CAPS LOCK correctly
      if (modifiers != 0 && modifiers != K_SHIFTFLAG) {
        return 0;
      }

      KMX_BOOL shifted = modifiers == K_SHIFTFLAG ? 1 : 0;

      if (vk == VK_SPACE) {
        // Override for space because it is the same for
        // shifted and unshifted.
        return 32;
      }

      for (int i = 0; chToVKey[i].vkey; i++) {
        if (chToVKey[i].vkey == vk && chToVKey[i].shifted == shifted) {
          return i + 32;
        }
      }
      return 0;
    }

    km_kbp_status kmx_processor::process_event(km_kbp_state *state, km_kbp_virtual_key vk, uint16_t modifier_state) {

      km_kbp_context_item *items;
      km_kbp_status status = km_kbp_context_get(km_kbp_state_context(state), &items);
      if (status != KM_KBP_STATUS_OK) return status;

      /* Construct a context buffer from the items */
      // for each context item, turn into (a) single km_kbp_cp, (b) pair of km_kbp_cp (SMP chr), or (c) UC_SENTINEL, CODE_DEADKEY, dk_value
      //TODO kmx.GetContext()->Set(ctx);

      km_kbp_context_items_dispose(items);

      // Convert VK to US char
      uint16_t ch = VKeyToChar(modifier_state, vk);

      kmx.GetActions()->ResetQueue();
      kmx.ProcessEvent(vk, modifier_state, ch);

      //TODO build list of actions from kmx stored actions
      // And run the actions

      return 0;
    }

    constexpr km_kbp_attr const engine_attrs = {
      256,
      KM_KBP_LIB_CURRENT,
      KM_KBP_LIB_AGE,
      KM_KBP_LIB_REVISION,
      KM_KBP_TECH_KMX,
      "SIL International"
    };

    km_kbp_attr const * kmx_processor::get_attrs() const {
      //TODO
      return &engine_attrs;
    }

  } // namespace kbp
} // namespace km