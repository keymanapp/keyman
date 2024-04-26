#pragma once

#include <keyman/keyman_core_api.h>
#include "kmx_file.h"

namespace km {
namespace core {

static const auto vkey_should_invalidate_context_count = 256;
/** true for any vkeys which require a context reset (such as frame keys) */
extern bool vkey_should_invalidate_context[vkey_should_invalidate_context_count];

/** @return true for any vkeys which require a context reset (such as frame keys) */
inline bool vkey_should_contextreset(km_core_virtual_key vk) {
  return ((vk < vkey_should_invalidate_context_count) && vkey_should_invalidate_context[vk]);
}

/** @return true for modifier states that are involved with context reset */
inline bool modifier_should_contextreset(uint16_t modifier_state) {
    // ctrl or alt
    return (modifier_state & (K_CTRLFLAG | K_ALTFLAG | LCTRLFLAG | RCTRLFLAG | LALTFLAG | RALTFLAG));
}


}
}
