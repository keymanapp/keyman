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
#include <algorithm>
#include <iterator>
#include <sstream>
#include <unordered_map>
#include <vector>

#include <keyman/keyboardprocessor.h>

#include "option.hpp"
#include "json.hpp"


size_t km_kbp_options_set_size(km_kbp_options_set const *opts)
{
  return opts->target.size();
}


km_kbp_option const *km_kbp_options_set_lookup(km_kbp_options_set const * opts,
                                               const char *key)
{
  auto i = opts->target.find(key);
  if (i == opts->target.end())
    return nullptr;

  return opts->export_option(i->first.c_str(), i->second.c_str());
}


km_kbp_status km_kbp_options_set_update(km_kbp_options_set *opts, km_kbp_option const *opt)
{
  try
  {
    opts->update(opt);
  }
  catch (std::bad_alloc) { return KM_KBP_STATUS_NO_MEM; }

  return KM_KBP_STATUS_OK;
}

// This function doesn't need to use the json pretty printer for such a simple
//  list of key:value pairs but it's a good introduction to it.
km_kbp_status km_kbp_options_set_to_json(km_kbp_options_set const *opts, char *buf, size_t *space)
{
  assert(opts);
  if (!opts)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  std::stringstream _buf;
  json jo(_buf);

  try
  {
    jo << opts->target;
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

namespace
{
  constexpr char const * const scope_name_lut[] = {
    "unknown",
    "keyboard",
    "environment",
  };
}

json & operator << (json &j, km::kbp::options_set const &opts)
{
  j << json::object
    << "scope" << scope_name_lut[opts.scope()]
    << "options" << json::object;
  for (auto & opt: opts)
    j << opt.first << opt.second;
  j << json::close;
  j << json::close;

  return j;
}
