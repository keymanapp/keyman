/*
  Copyright:    © 2018 SIL International.
  Description:  This is a test implementation of the keyboard processor API to
                enable testing API clients against a basic keyboard and give
                them something to link against and load.
                TODO: Add a mecahnism to trigger output of PERSIST_OPT &
                RESET_OPT actions items, options support and context matching.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Initial implementation.
*/

#include "mock/mock_processor.hpp"
#include "state.hpp"

namespace
{

  // An table of VKEY to character sequence mappings, one per shift state.
  // An empty string inidicates no mapping.
  constexpr char const table[2][256][4] = {
    {
      /*00-0F*/"","","","","","","","","","\t","","","","\015","","",
      /*10-1F*/"","","","","","","","","","","","\033","","","","",
      /*20-2F*/" ","","","","","","","","","","","","","","","",
      /*30-3F*/"0","1","2","3","4","5","6","7","8","9","","","","","","",
      /*40-4F*/"","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o",
      /*50-5F*/"p","q","r","s","t","u","v","w","x","y","z","","","","","",
      /*60-6F*/"0","1","2","3","4","5","6","7","8","9","*","+","","-",".","/",
      /*70-7F*/"f1","f2","f3","f4","f5","f6","f7","f8","f9","f10","f11","f12","f13","f14","f15","f16",
      /*80-8F*/"f17","f18","f19","f20","f24","","","","","","","","","","","",
      /*90-9F*/"","","","","","","","","","","","","","","","",
      /*A0-AF*/"","","","","","","","","","","","","","","","",
      /*B0-BF*/"","","","","","","","","","",";","=",",","-",".","/",
      /*C0-CF*/"`","","","","","","","","","","","","","","","",
      /*D0-DF*/"","","","","","","","","","","","[","\\","]","'","",
      /*E0-EF*/"","","","","","","","","","","","","","","","",
      /*F0-FF*/"","","","","","","","","","","","","","","","",
    },
    {
      /*00-0F*/"","","","","","","","","","\t","","","","\015","","",
      /*10-1F*/"","","","","","","","","","","","\033","","","","",
      /*20-2F*/" ","","","","","","","","","","","","","","","",
      /*30-3F*/")","!","@","#","$","%","^","&","*","(","","","","","","",
      /*40-4F*/"","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O",
      /*50-5F*/"P","Q","R","S","T","U","V","W","X","Y","Z","","","","","",
      /*60-6F*/"","","","","","","","","","","","","","","","",
      /*70-7F*/"","","","","","","","","","","","","","","","",
      /*80-8F*/"","","","","","","","","","","","","","","","",
      /*90-9F*/"","","","","","","","","","","","","","","","",
      /*A0-AF*/"","","","","","","","","","","","","","","","",
      /*B0-BF*/"","","","","","","","","","",":","+","<","_",">","?",
      /*C0-CF*/"~","","","","","","","","","","","","","","","",
      /*D0-DF*/"","","","","","","","","","","","{","|","}","\"","",
      /*E0-EF*/"","","","","","","","","","","","","","","","",
      /*F0-FF*/"","","","","","","","","","","","","","","","",
    }
  };


  constexpr km_core_attr const engine_attrs = {
    256,
    KM_CORE_LIB_CURRENT,
    KM_CORE_LIB_AGE,
    KM_CORE_LIB_REVISION,
    KM_CORE_TECH_MOCK,
    "SIL International"
  };

}

namespace km {
  namespace core
  {
    mock_processor::mock_processor(core::path const & path)
    : abstract_processor(
        keyboard_attributes(path.stem(), u"3.145", {
          option{KM_CORE_OPT_KEYBOARD, u"__test_point", u"not tiggered"},
        })),
      _options({
          {u"\x01__test_point", u"not tiggered"},
          {u"\x02hello", u"-"}
      })
    {
    }

    char16_t const * mock_processor::lookup_option(km_core_option_scope scope,
                                    std::u16string const & key) const
    {
      auto i = _options.find(char16_t(scope) + key);
      return i != _options.end() ? i->second.c_str() : nullptr;
    }

    option mock_processor::update_option(km_core_option_scope scope,
                       std::u16string const & key,
                       std::u16string const & value)
    {
      auto result = _options.emplace(char16_t(scope) + key, value);
      if ( !result.second && result.first == _options.end()) {
         return option();
      }
      // key already existed so just update the value
      if(!result.second) {
        result.first->second = value;
      }
      persisted_store()[key] = value;
      return option(scope, key, result.first->second);
    }

    km_core_status
    mock_processor::process_queued_actions(
      km_core_state *state
    ) {
      assert(state);
      if (!state)
        return KM_CORE_STATUS_INVALID_ARGUMENT;
      // TODO Implement
      return KM_CORE_STATUS_OK;
    }

    bool mock_processor::queue_action(
      km_core_state * state,
      km_core_action_item const* action_item
    )
    {
      assert(state);
      assert(action_item);
      if ((!state) || (!action_item))
        return false;
      return false;
    }

    km_core_status
    mock_processor::process_event(
      km_core_state *state,
      km_core_virtual_key vk,
      uint16_t modifier_state,
      uint8_t is_key_down,
      uint16_t /* event_flags */
    ) {
      assert(state);
      if (!state)
        return KM_CORE_STATUS_INVALID_ARGUMENT;

      if (!is_key_down) {
        // TODO: Implement caps lock handling
        return KM_CORE_STATUS_OK;
      }

      try {
        // At the start of every process_event always clear the action_items
        state->actions().clear();

        switch (vk)
        {
        case KM_CORE_VKEY_BKSP:
          state->context().pop_back();
          state->actions().push_backspace(KM_CORE_BT_UNKNOWN); // Assuming we don't know the character
          break;

        case KM_CORE_VKEY_F2:
        {
          state->actions().push_persist(
            update_option(KM_CORE_OPT_KEYBOARD,
                        u"__test_point",
                        u"F2 pressed test save."));
          break;
        }

        case KM_CORE_VKEY_F4:
          state->context().push_marker(KM_CORE_VKEY_QUOTE);
          state->actions().push_marker(KM_CORE_VKEY_QUOTE);
          break;

        default:
        {
          auto shift_state = bool(modifier_state & KM_CORE_MODIFIER_SHIFT);
          // Only process further one of the shift states has something to output.
          if (table[0][vk][0] || table[1][vk][0])
          {
            auto char_seq = table[shift_state][vk];

            for (auto c = char_seq; *c; ++c)
            {
              km_core_usv usv = *c;
              state->context().push_character(usv);
              state->actions().push_character(usv);
            }
            state->actions().commit();

            return KM_CORE_STATUS_OK;
          }

          // Both shift states output nothing, generate an alert.
          state->actions().push_alert();
          break;
        }
        }

        state->actions().commit();
      } catch (std::bad_alloc &) {
        state->actions().clear();
        return KM_CORE_STATUS_NO_MEM;
      }

      return KM_CORE_STATUS_OK;
    }

    km_core_attr const & mock_processor::attributes() const {
      return engine_attrs;
    }

    km_core_keyboard_key  * mock_processor::get_key_list() const {
      km_core_keyboard_key* key_list = new km_core_keyboard_key(KM_CORE_KEYBOARD_KEY_LIST_END);
      return key_list;
    }

    km_core_keyboard_imx  * mock_processor::get_imx_list() const {
      km_core_keyboard_imx* imx_list = new km_core_keyboard_imx(KM_CORE_KEYBOARD_IMX_END);
      return imx_list;
    }

    km_core_context_item * mock_processor::get_intermediate_context() {
      km_core_context_item *citems = new km_core_context_item(KM_CORE_CONTEXT_ITEM_END);
      return citems;
    }

    km_core_status mock_processor::validate() const { return KM_CORE_STATUS_OK; }

    km_core_status null_processor::validate() const { return KM_CORE_STATUS_INVALID_ARGUMENT; }

    /**
     * Returns true if the data starts with 'MOCK'.
     *
     * @param data  the keyboard blob
     * @return true if the processor can handle the keyboard, otherwise false.
     */
    bool mock_processor::is_handled(const std::vector<uint8_t>& data) {
      if (data.empty()) {
        return false;
      }

      if (data.size() < 4) {  // a MOCK file is at least 4 bytes (MOCK)
        return false;
      }

      return ((char*)data.data())[0] == 'M' && ((char*)data.data())[1] == 'O' && ((char*)data.data())[2] == 'C' && ((char*)data.data())[3] == 'K';
    }
  }  // namespace core
} // namespace km
