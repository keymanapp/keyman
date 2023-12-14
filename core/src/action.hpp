/*
  Copyright:    © 2023 SIL International.
  Description:  Internal actions methods for Keyman Core
  Create Date:  23 Oct 2023
  Authors:      Marc Durdin (MCD)
  History:      23 Oct 2023 - MCD - Initial implementation
*/

#pragma once

#include <keyman/keyman_core_api.h>
#include <state.hpp>

namespace km {
namespace core
{
  km_core_actions const *action_item_list_to_actions_object(
    km_core_action_item const *action_items
  );
} // namespace core
} // namespace km
