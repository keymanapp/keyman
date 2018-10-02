#include <cassert>
#include <algorithm>
#include <iterator>
#include <vector>

#include <keyboardprocessor.h>

#include "context.hpp"
#include "utfcodec.hpp"

struct km_kbp_context : public km::kbp::context {};

km_kbp_status
km_kbp_context_items_from_utf16(km_kbp_cp const *text,
                                km_kbp_context_item **out_ptr)
{
  *out_ptr = nullptr;
  std::vector<km_kbp_context_item> res;

  try
  {
    for (auto i = utf16::const_sentinal_iterator(text), end = decltype(i)();
         i != end; ++i)
    {
      if(i.error())   return KM_KBP_STATUS_INVALID_ARGUMENT;
      res.emplace_back(km_kbp_context_item {KM_KBP_CT_CHAR, {0,}, {*i}});
    }
    res.emplace_back(km_kbp_context_item {KM_KBP_CT_END, {0,}, {0}});
    *out_ptr = new km_kbp_context_item[res.size()];
    std::copy(res.begin(), res.end(), *out_ptr);
  }
  catch(std::bad_alloc)
  {
    return KM_KBP_STATUS_NO_MEM;
  }

  return KM_KBP_STATUS_OK;
}


size_t km_kbp_context_items_to_utf16(km_kbp_context_item const *ci,
                                     km_kbp_cp *buf, size_t buf_size)
{
  assert(ci != nullptr);
  if (!ci) return 0;

  if (buf)
  {
    auto i = utf16::iterator(buf);
    auto const e = utf16::iterator(buf + buf_size);
    for (;i != e && ci->type != KM_KBP_CT_END; ++ci, ++i)
    {
      if (ci->type == KM_KBP_CT_CHAR)
        *i = ci->character;
    }

    return buf_size - (e - i);
  }
  else
  {
    auto n = 0;

    do
      if (ci->type == KM_KBP_CT_CHAR)
        ++n += int(ci->character >= 0x10000);
    while(ci++->type != KM_KBP_CT_END);
    return n;
  }
}


void km_kbp_context_items_dispose(km_kbp_context_item *ci)
{
  delete [] ci;
}


km_kbp_status km_kbp_context_set(km_kbp_context *ctxt, km_kbp_context_item const *ci)
{
    ctxt->clear();
    return km_kbp_context_append(ctxt, ci);
}


km_kbp_context_item const * km_kbp_context_get(km_kbp_context const *ctxt)
{
  return ctxt->data();
}


void km_kbp_context_clear(km_kbp_context *ctxt)
{
  ctxt->clear();
}


size_t km_kbp_context_length(km_kbp_context *ctxt)
{
  return ctxt->size();
}


km_kbp_status km_kbp_context_append(km_kbp_context *ctxt,
                                    km_kbp_context_item const *ci)
{
  try
  {
    while(ci->type != KM_KBP_CT_END) ctxt->emplace_back(*ci++);
  } catch(std::bad_alloc) {
    return KM_KBP_STATUS_NO_MEM;
  }

  return KM_KBP_STATUS_OK;
}


void km_kbp_context_shrink(km_kbp_context *ctxt, size_t num,
                           km_kbp_context_item const * ci)
{
  ctxt->resize(ctxt->size() - std::max(num, ctxt->size()));

  while(num-- && ci->type != KM_KBP_CT_END)
    ctxt->emplace_front(*ci++);
}
