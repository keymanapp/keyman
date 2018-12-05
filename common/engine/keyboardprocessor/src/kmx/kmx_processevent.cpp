#include <keyman/keyboardprocessor.h>
#include "processor.hpp"
#include "state.hpp"
#include "keyboard.hpp"

namespace km {
  namespace kbp
  {

    km_kbp_status kmx_processor::validate() const {
      return _valid ? KM_KBP_STATUS_OK : KM_KBP_STATUS_INVALID_KEYBOARD;
    }

    kmx_processor::kmx_processor(km_kbp_keyboard_attrs const * kb_) : abstract_processor(kb_) {
      km::kbp::keyboard *kb = const_cast<km::kbp::keyboard *>(static_cast<km::kbp::keyboard const *>(kb_));

      std::filesystem::path p = kb->folder_path;
      p /= kb->id;
      p.replace_extension(".kmx");
      _valid = (bool) _kmx.Load(p.native().c_str());

      if (_valid) {
        _kmx.GetOptions()->Init(kb->default_opts());
      }
    }

    void kmx_processor::init_state(std::vector<km_kbp_option_item> *default_env) {
      _kmx.GetEnvironment()->Init(default_env);
    }

    void kmx_processor::update_option(km_kbp_state *state, km_kbp_option_scope scope, std::u16string const & key, std::u16string const & value) {
      switch(scope) {
        case KM_KBP_OPT_KEYBOARD:
          _kmx.GetOptions()->Load(km_kbp_state_options(state), key);
          break;
        case KM_KBP_OPT_ENVIRONMENT:
          _kmx.GetEnvironment()->Load(key, value);
          break;
        default:
          break;
      }
    }

    km_kbp_status kmx_processor::process_event(km_kbp_state *state, km_kbp_virtual_key vk, uint16_t modifier_state) {
      // Construct a context buffer from the items

      std::u16string ctxt;
      auto cp = state->context();
      for (auto c = cp.begin(); c != cp.end(); c++) {
        switch (c->type) {
        case KM_KBP_CT_CHAR:
          if (Uni_IsSMP(c->character)) {
            ctxt += Uni_UTF32ToSurrogate1(c->character);
            ctxt += Uni_UTF32ToSurrogate2(c->character);
          }
          else {
            ctxt += (km_kbp_cp) c->character;
          }
          break;
        case KM_KBP_CT_MARKER:
          assert(c->marker > 0);
          ctxt += UC_SENTINEL;
          ctxt += CODE_DEADKEY;
          ctxt += c->marker; 
          break;
        }
      }

      _kmx.GetContext()->Set(ctxt.c_str());
      _kmx.GetActions()->ResetQueue();
      state->actions.clear();

      if (!_kmx.ProcessEvent(state, vk, modifier_state)) {
        // We need to output the default keystroke
        state->actions.emplace_back(km_kbp_action_item{ KM_KBP_IT_EMIT_KEYSTROKE, {0,}, {0} });
      }

      for (auto i = 0; i < _kmx.GetActions()->Length(); i++) {
        auto a = _kmx.GetActions()->Get(i);
        switch (a.ItemType) {
        case QIT_CAPSLOCK:
          //TODO: add Caps Event
          //dwData = 0 == off; 1 == on
          //state->actions.emplace_back(km_kbp_action_item{ KM_KBP_IT_CAPSLOCK, {0,}, {0} });
          break;
        case QIT_VKEYDOWN:
        case QIT_VKEYUP:
        case QIT_VSHIFTDOWN:
        case QIT_VSHIFTUP:
          //TODO: eliminate??
          break;
        case QIT_CHAR:          
          state->context().emplace_back(km_kbp_context_item{ KM_KBP_CT_CHAR, {0,}, {(km_kbp_usv)a.dwData} });
          state->actions.emplace_back(km_kbp_action_item{ KM_KBP_IT_CHAR, {0,}, {(km_kbp_usv)a.dwData} });
          break;
        case QIT_DEADKEY:
          state->context().emplace_back(km_kbp_context_item{ KM_KBP_CT_MARKER, {0,}, {(km_kbp_usv)a.dwData} });
          state->actions.emplace_back(km_kbp_action_item{ KM_KBP_IT_MARKER, {0,}, {(uintptr_t)a.dwData} });
          break;
        case QIT_BELL:
          state->actions.emplace_back(km_kbp_action_item{ KM_KBP_IT_ALERT, {0,}, {0} });
          break;
        case QIT_BACK:
          switch (a.dwData) {
          case BK_DEFAULT:
            // This only happens if we know we have context to delete. Last item must be a character
            assert(!state->context().empty() && state->context().back().type != KM_KBP_IT_MARKER);
            if (!state->context().empty()) state->context().pop_back();
            state->actions.emplace_back(km_kbp_action_item{ KM_KBP_IT_BACK, {0,}, {0} });
            break;
          case BK_DEADKEY:
            // This only happens if we know we have context to delete. Last item must be a deadkey
            assert(!state->context().empty() && state->context().back().type == KM_KBP_IT_MARKER);
            if (!state->context().empty()) state->context().pop_back();
            break;
          case BK_BACKSPACE:
            // User-initiated backspace. We need to delete deadkeys from context, both sides of the character deleted
            while (!state->context().empty() && state->context().back().type == KM_KBP_IT_MARKER) state->context().pop_back();
            if (!state->context().empty()) {
              state->context().pop_back();
              while (!state->context().empty() && state->context().back().type == KM_KBP_IT_MARKER) state->context().pop_back();
            }
            // Even if context is empty, we send the backspace event, because we may not
            // know the context.
            state->actions.emplace_back(km_kbp_action_item{ KM_KBP_IT_BACK, {0,}, {0} }); 
            break;
          default:
            assert(false);
          }
          break;
        case QIT_INVALIDATECONTEXT:
          state->actions.emplace_back(km_kbp_action_item{ KM_KBP_IT_INVALIDATE_CONTEXT, {0,}, {0} });
          break;
        default:
          //std::cout << "Unexpected item type " << a.ItemType << ", " << a.dwData << std::endl;
          assert(false);
        }
      }

      state->actions.emplace_back(km_kbp_action_item{ KM_KBP_IT_END, {0,}, {0} });

      return KM_KBP_STATUS_OK;
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