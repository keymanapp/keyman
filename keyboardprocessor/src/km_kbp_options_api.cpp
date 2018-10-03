#include <cassert>
#include <algorithm>
#include <iterator>
#include <sstream>
#include <unordered_map>
#include <vector>

#include <keyboardprocessor.h>

#include "option.hpp"
#include "json.hpp"


struct km_kbp_option_set : public km::kbp::option_set
{
  km_kbp_option_set(km_kbp_option_scope s)
  : option_set(s),
    _last_lookup {s, nullptr, nullptr}
  {}

  km_kbp_option const * cache_lookup(char const * k, char const * v) const {
    _last_lookup.key = k;
    _last_lookup.value = v;
    return &_last_lookup;
  }

private:
  km_kbp_option mutable _last_lookup;
};


size_t km_kbp_options_set_size(km_kbp_option_set const *opts)
{
  return opts->size();
}


km_kbp_option const *km_kbp_options_set_lookup(km_kbp_option_set const * opts,
                                               const char *key)
{
  auto i = opts->find(key);
  if (i == opts->end())
    return nullptr;

  return opts->cache_lookup(i->first.c_str(), i->second.c_str());
}


km_kbp_status km_kbp_options_set_update(km_kbp_option_set *opts, km_kbp_option const *opt)
{
  try
  {
    opts->update(opt);
  }
  catch (std::bad_alloc) { return KM_KBP_STATUS_NO_MEM; }

  return KM_KBP_STATUS_OK;
}

// This simple function doesn't need to use the json pretty printer for such a
//  simple list of key:value pairs but it's a good introduction to it.
km_kbp_status km_kbp_options_set_to_json(km_kbp_option_set const *opts, char *buf, size_t *space)
{
  std::stringstream _buf;
  json jo(_buf);

  try
  {
    // Pretty print the document.
    jo << json::array;
    for (auto & opt: *opts)
      jo << json::flat << json::object
        << opt.first
        << opt.second
        << json::close;
    jo << json::close;
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
