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

#include <keyman/keyboardprocessor.h>
#include "processor.hpp"

#include "json.hpp"
#include "state.hpp"


size_t
km_kbp_options_list_size(km_kbp_option_item const *opts)
{
  assert(opts);
  if (!opts)  return 0;

  auto n = 0;
  while (opts->key) ++n;

  return n;
}


km_kbp_status
km_kbp_options_lookup(km_kbp_state const *state,
                      uint8_t scope, km_kbp_cp const *key,
                      km_kbp_cp const **value_out)
{
  assert(state); assert(key); assert(value_out);
  if (!state || !key || !value_out)  return KM_KBP_STATUS_INVALID_ARGUMENT;

  if (scope == KM_KBP_OPT_UNKNOWN || scope > KM_KBP_OPT_MAX_SCOPES)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  auto opts = km_kbp_state_options(const_cast<km_kbp_state *>(state));

  // Copy the internal value to our new buffer
  km_kbp_cp const *internal_value = opts->lookup(km_kbp_option_scope(scope), key);
  if (!internal_value)
  {
    return KM_KBP_STATUS_KEY_ERROR;
  }
  std::u16string const &value = internal_value;

  km_kbp_cp *valuep;
  try
  {
    valuep = new km_kbp_cp[value.size() + 1];
  }
  catch (std::bad_alloc)
  {
    return KM_KBP_STATUS_NO_MEM;
  }
  std::copy(value.begin(), value.end(), valuep);
  valuep[value.size()] = u'\0';

  *value_out = valuep;

  return KM_KBP_STATUS_OK;
}


km_kbp_status
km_kbp_options_update(km_kbp_state *state, km_kbp_option_item const *opt)
{
  assert(state); assert(opt);
  if (!state|| !opt)  return KM_KBP_STATUS_INVALID_ARGUMENT;

  auto opts = km_kbp_state_options(state);

  try
  {
    for (;opt->key; ++opt)
    {
      if (opt->scope == KM_KBP_OPT_UNKNOWN || opt->scope > KM_KBP_OPT_MAX_SCOPES)
        return KM_KBP_STATUS_INVALID_ARGUMENT;

      if (!opts->assign(state, km_kbp_option_scope(opt->scope), opt->key, opt->value))
        return KM_KBP_STATUS_KEY_ERROR;
    }
  } 
  catch (std::bad_alloc)
  {
    return KM_KBP_STATUS_NO_MEM;
  }

  return KM_KBP_STATUS_OK;
}

// This function doesn't need to use the json pretty printer for such a simple
//  list of key:value pairs but it's a good introduction to it.
km_kbp_status
km_kbp_options_to_json(km_kbp_options const *opts, char *buf, size_t *space)
{
  assert(opts); assert(space);
  if (!opts || !space)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  std::stringstream _buf;
  json jo(_buf);

  try
  {
    jo << *opts;
  }
  catch (std::bad_alloc)
  {
    *space = 0;
    return KM_KBP_STATUS_NO_MEM;
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
  return KM_KBP_STATUS_OK;
}

void
km_kbp_cp_dispose(km_kbp_cp const *cp)
{
  delete [] cp;
}
