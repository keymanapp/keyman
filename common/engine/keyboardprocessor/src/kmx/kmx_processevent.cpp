#include <keyman/keyboardprocessor.h>
#include "state.hpp"
#include "kmx/kmx_processevent.hpp"

namespace km {
  namespace kbp
  {

    km_kbp_status kmx_processor::validate() const {
      return _valid ? KM_KBP_STATUS_OK : KM_KBP_STATUS_INVALID_KEYBOARD;
    }

    kmx_processor::kmx_processor(kbp::path p)
    {
      p.replace_extension(".kmx");
      _valid = bool(_kmx.Load(p.c_str()));

      keyboard_attributes::options_store defaults;
      if (_valid)
        _kmx.GetOptions()->Init(defaults);

      for (auto const & opt: defaults)
      {
        if (!opt.empty() && opt.scope == KM_KBP_OPT_KEYBOARD  )
          persisted_store()[opt.key] = opt.value;
      }
      // Fill out attributes
      auto v = _kmx.GetKeyboard()->Keyboard->version;
      auto vs = std::to_string(v >> 16) + "." + std::to_string(v & 0xffff);

      _attributes = keyboard_attributes(static_cast<std::u16string>(p.stem()),
                      std::u16string(vs.begin(), vs.end()), p.parent(), defaults);
    }


    char16_t const * kmx_processor::lookup_option(km_kbp_option_scope scope, std::u16string const & key) const
    {
      char16_t const * pValue = nullptr;
      switch(scope)
      {
        case KM_KBP_OPT_KEYBOARD:
          pValue = _kmx.GetOptions()->LookUp(key);
          break;
        case KM_KBP_OPT_ENVIRONMENT:
          pValue = _kmx.GetEnvironment()->LookUp(key);
          break;
        default:
          break;
      }

      return pValue ? pValue : nullptr;
    }

    option kmx_processor::update_option(km_kbp_option_scope scope, std::u16string const & key, std::u16string const & value)
    {
      switch(scope) {
        case KM_KBP_OPT_KEYBOARD:
          _kmx.GetOptions()->Set(key, value);
          persisted_store()[key] = value;
          break;
        case KM_KBP_OPT_ENVIRONMENT:
          _kmx.GetEnvironment()->Load(key, value);
          break;
        default:
          return option();
          break;
      }

      return option(scope, key, value);
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
      state->actions().clear();

      if (!_kmx.ProcessEvent(state, vk, modifier_state)) {
        // We need to output the default keystroke
        state->actions().push_emit_keystroke();
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
          state->context().push_character(a.dwData);
          state->actions().push_character(a.dwData);
          break;
        case QIT_DEADKEY:
          state->context().push_marker(a.dwData);
          state->actions().push_marker(a.dwData);
          break;
        case QIT_BELL:
          state->actions().push_alert();
          break;
        case QIT_BACK:
          switch (a.dwData) {
          case BK_DEFAULT:
            // This only happens if we know we have context to delete. Last item must be a character
            assert(!state->context().empty() && state->context().back().type != KM_KBP_IT_MARKER);
            if (!state->context().empty()) state->context().pop_back();
            state->actions().push_backspace();
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
            state->actions().push_backspace();
            break;
          default:
            assert(false);
          }
          break;
        case QIT_INVALIDATECONTEXT:
          state->actions().push_invalidate_context();
          break;
        default:
          //std::cout << "Unexpected item type " << a.ItemType << ", " << a.dwData << std::endl;
          assert(false);
        }
      }

      state->actions().commit();

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

    km_kbp_attr const & kmx_processor::attributes() const {
      return engine_attrs;
    }

  } // namespace kbp
} // namespace km
