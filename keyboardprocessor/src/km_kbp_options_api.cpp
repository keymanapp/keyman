#include <cassert>
#include <algorithm>
#include <iterator>
#include <sstream>
#include <unordered_map>
#include <vector>

#include <keyboardprocessor.h>

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
    std::copy(doc.begin(), doc.end(), buf);
    buf[doc.size()] = 0;
  }

  // Return space needed/used.
  *space = doc.size();
  return KM_KBP_STATUS_OK;
}

namespace
{
  constexpr char const * const scope_name_lut[] = {
    "unknown",
    "enviroment",
    "keyboard"
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
