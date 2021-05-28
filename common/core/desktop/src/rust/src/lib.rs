/*
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


  assert(state);
  if (!state)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  try
  {
    // At the start of every process_event allways clear the action_items
    state->actions().clear();

    switch (vk)
    {
    case KM_KBP_VKEY_BKSP:
      state->context().pop_back();
      state->actions().push_backspace();
      break;

    case KM_KBP_VKEY_F2:
    {
      state->actions().push_persist(
        update_option(KM_KBP_OPT_KEYBOARD,
                    u"__test_point",
                    u"F2 pressed test save."));
      break;
    }

    case KM_KBP_VKEY_F4:
      state->context().push_marker(KM_KBP_VKEY_QUOTE);
      state->actions().push_marker(KM_KBP_VKEY_QUOTE);
      break;

    default:
    {
      auto shift_state = bool(modifier_state & KM_KBP_MODIFIER_SHIFT);
      // Only process further one of the shift states has something to output.
      if (table[0][vk][0] || table[1][vk][0])
      {
        auto char_seq = table[shift_state][vk];

        for (auto c = char_seq; *c; ++c)
        {
          km_kbp_usv usv = *c;
          state->context().push_character(usv);
          state->actions().push_character(usv);
        }
        state->actions().commit();

        return KM_KBP_STATUS_OK;
      }

      // Both shift states output nothing, generate an alert.
      state->actions().push_alert();
      break;
    }
    }

    state->actions().commit();
  }
  catch (std::bad_alloc &)
  {
    state->actions().clear();
    return KM_KBP_STATUS_NO_MEM;
  }

  return KM_KBP_STATUS_OK;
  */

#[no_mangle]
pub extern "C" fn rust_mock_process_event(_vk: u16, _modifier: u16) -> u32 {
  return 0; //KM_KBP_STATUS_OK
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_rust_mock_process_event() {
    assert_eq!(0, rust_mock_process_event(0, 0));
  }
}
