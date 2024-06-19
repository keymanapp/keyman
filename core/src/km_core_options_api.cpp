/*
  Copyright:    © 2018 SIL International.
  Description:  Implementation of the options API functions using internal
                data structures and functions.
  Create Date:  27 Sep 2018
  Authors:      Tim Eves (TSE)
  History:      27 Sep 2018 - TSE - Initial implementation.
                5  Oct 2018 - TSE - Refactor out adaptor and internal classes
                                    into option.hpp
*/
#include <cassert>
#include <sstream>

#include "keyman_core.h"

#include "processor.hpp"

#include "jsonpp.hpp"
#include "state.hpp"


size_t
km_core_options_list_size(km_core_option_item const *opts)
{
  assert(opts);
  if (!opts)  return 0;

  auto n = 0;
  for (; opts->key; ++opts) {
    ++n;
  }

  return n;
}


km_core_status
km_core_state_option_lookup(km_core_state const *state,
                      uint8_t scope, km_core_cu const *key,
                      km_core_cu const **value_out)
{
  assert(state); assert(key); assert(value_out);
  if (!state || !key || !value_out)  return KM_CORE_STATUS_INVALID_ARGUMENT;

  if (scope == KM_CORE_OPT_UNKNOWN || scope > KM_CORE_OPT_MAX_SCOPES)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  auto & processor = state->processor();

  *value_out = processor.lookup_option(km_core_option_scope(scope), key);
  if (!*value_out)  return KM_CORE_STATUS_KEY_ERROR;

  return KM_CORE_STATUS_OK;
}


km_core_status
km_core_state_options_update(km_core_state *state, km_core_option_item const *opt)
{
  assert(state); assert(opt);
  if (!state|| !opt)  return KM_CORE_STATUS_INVALID_ARGUMENT;

  auto & processor = state->processor();

  try
  {
    for (;opt->key; ++opt)
    {
      if (opt->scope == KM_CORE_OPT_UNKNOWN || opt->scope > KM_CORE_OPT_MAX_SCOPES)
        return KM_CORE_STATUS_INVALID_ARGUMENT;

      if (processor.update_option(
            km_core_option_scope(opt->scope),
            opt->key,
            opt->value).empty())
        return KM_CORE_STATUS_KEY_ERROR;
    }
  }
  catch (std::bad_alloc &)
  {
    return KM_CORE_STATUS_NO_MEM;
  }

  return KM_CORE_STATUS_OK;
}

// This function doesn't need to use the json pretty printer for such a simple
//  list of key:value pairs but it's a good introduction to it.
km_core_status
km_core_state_options_to_json(km_core_state const *state, char *buf, size_t *space)
{
  assert(state); assert(space);
  if (!state || !space)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  std::stringstream _buf;
  json jo(_buf);

  try
  {
// TODO: Fix
//    jo << state->options();
  }
  catch (std::bad_alloc &)
  {
    *space = 0;
    return KM_CORE_STATUS_NO_MEM;
  }

  // Fetch the finished doc and copy it to the buffer if there enough space.
  auto const doc = _buf.str();
  if (buf && *space > doc.size())
  {
    doc.copy(buf, *space);
    buf[doc.size()] = 0;
  }

  // Return space needed/used.
  *space = doc.size()+1;
  return KM_CORE_STATUS_OK;
}
