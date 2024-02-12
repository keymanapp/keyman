/*
  Copyright:    © 2023 SIL International.
  Description:  Internal actions methods for Keyman Core
  Create Date:  23 Oct 2023
  Authors:      Marc Durdin (MCD)
  History:      23 Oct 2023 - MCD - Initial implementation
*/

#pragma once

#include "keyman_core.h"
#include <state.hpp>

namespace km {
namespace core
{
  bool action_item_list_to_actions_object(
    km_core_action_item const *action_items,
    km_core_actions *actions
  );

  bool actions_normalize(
    /* in */      context const *cached_context,
    /* in, out */ context *app_context,
    /* in, out */ km_core_actions &actions
  );

  bool actions_update_app_context_nfu(
    /* in */      context const *cached_context,
    /* in, out */ context *app_context
  );

  void actions_dispose(
    km_core_actions const & actions
  );

  km_core_usv const *get_deleted_context(
    context const &app_context,
    unsigned int code_points_to_delete
  );

} // namespace core
} // namespace km
