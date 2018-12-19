/*
  Copyright:    © 2018 SIL International.
  Description:  Implementation of the context API functions using internal
                data structures and functions.
  Create Date:  27 Sep 2018
  Authors:      Tim Eves (TSE)
  History:      27 Sep 2018 - TSE - Initial implementation.
                5  Oct 2018 - TSE - Refactor out adaptor and internal classes
                                    into context.hpp
*/
#include <cassert>
#include <algorithm>
#include <vector>

#include <keyman/keyboardprocessor.h>

#include "context.hpp"
#include "json.hpp"
#include "utfcodec.hpp"

namespace {
  template<class U>
  km_kbp_status
  _context_items_from(typename U::codeunit_t const *text,
                      km_kbp_context_item **out_ptr)
  {
    assert(text); assert(out_ptr);
    if (!text || !out_ptr)  return KM_KBP_STATUS_INVALID_ARGUMENT;

    *out_ptr = nullptr;
    try
    {
      std::vector<km_kbp_context_item> res;

      for (auto i = typename U::const_sentinal_iterator(text); i; ++i)
      {
        if(i.error())   return KM_KBP_STATUS_INVALID_UTF;
        res.emplace_back(km_kbp_context_item {KM_KBP_CT_CHAR, {0,}, {*i}});
      }
      // Terminate the context_items array.
      res.emplace_back(km_kbp_context_item KM_KBP_CONTEXT_ITEM_END);

      *out_ptr = new km_kbp_context_item[res.size()];
      std::move(res.begin(), res.end(), *out_ptr);
    }
    catch(std::bad_alloc)
    {
      return KM_KBP_STATUS_NO_MEM;
    }

    return KM_KBP_STATUS_OK;
  }

  template<class U>
  km_kbp_status _context_items_to(km_kbp_context_item const *ci,
                                  typename U::codeunit_t *buf,
                                  size_t * sz_ptr)
  {
    assert(ci); assert(sz_ptr);
    if (!ci || !sz_ptr)   return KM_KBP_STATUS_INVALID_ARGUMENT;
    auto const buf_size = *sz_ptr;

    if (buf && buf_size > 0)
    {
      auto i = typename U::iterator(buf);
      auto const e = decltype(i)(buf + buf_size - 1);

      for (;i != e && ci->type != KM_KBP_CT_END; ++ci)
      {
        if (ci->type == KM_KBP_CT_CHAR)
        {
          *i = ci->character; ++i;
        }
      }
      *i = 0; // null terminate the UTF16 string.

      *sz_ptr = buf_size - (e - i);

      return ci->type == KM_KBP_CT_END
              ? KM_KBP_STATUS_OK
              : KM_KBP_STATUS_INSUFFICENT_BUFFER;
    }
    else
    {
      auto n = 0;
      typename U::codeunit_t cps[4/sizeof(typename U::codeunit_t)];

      do
      {
        if (ci->type == KM_KBP_CT_CHAR)
        {
          int8_t l;
          U::codec::put(cps, ci->character, l);
          n += l;
        }
      }
      while(ci++->type != KM_KBP_CT_END);

      *sz_ptr = n+1;
      return KM_KBP_STATUS_OK;
    }
  }
}

km_kbp_status
km_kbp_context_items_from_utf16(km_kbp_cp const *text,
                                km_kbp_context_item **out_ptr)
{
  return _context_items_from<utf16>(reinterpret_cast<utf16::codeunit_t const *>(text), out_ptr);
}


km_kbp_status
km_kbp_context_items_from_utf8(char const *text,
                                km_kbp_context_item **out_ptr)
{
  return _context_items_from<utf8>(reinterpret_cast<utf8::codeunit_t const *>(text), out_ptr);
}


km_kbp_status km_kbp_context_items_to_utf8(km_kbp_context_item const *ci,
                                            char *buf, size_t * sz_ptr)
{
  return _context_items_to<utf8>(ci,
            reinterpret_cast<utf8::codeunit_t *>(buf),
            sz_ptr);
}


km_kbp_status km_kbp_context_items_to_utf16(km_kbp_context_item const *ci,
                                            km_kbp_cp *buf, size_t * sz_ptr)
{
  return _context_items_to<utf16>(ci,
            reinterpret_cast<utf16::codeunit_t *>(buf),
            sz_ptr);
}


void km_kbp_context_items_dispose(km_kbp_context_item *ci)
{
  delete [] ci;
}


km_kbp_status km_kbp_context_set(km_kbp_context *ctxt, km_kbp_context_item const *ci)
{
    km_kbp_context_clear(ctxt);
    return km_kbp_context_append(ctxt, ci);
}


km_kbp_status km_kbp_context_get(km_kbp_context const *ctxt,
                                 km_kbp_context_item **out_ptr)
{
  assert(ctxt); assert(out_ptr);
  if (!ctxt || !out_ptr)   return KM_KBP_STATUS_INVALID_ARGUMENT;

  try
  {
    *out_ptr = new km_kbp_context_item[ctxt->size() + 1];
  }
  catch (std::bad_alloc)
  {
    return KM_KBP_STATUS_NO_MEM;
  }
  std::copy(ctxt->begin(), ctxt->end(), *out_ptr);
  (*out_ptr)[ctxt->size()].type = KM_KBP_CT_END;

  return KM_KBP_STATUS_OK;
}


void km_kbp_context_clear(km_kbp_context *ctxt)
{
  assert(ctxt);
  if (ctxt)
  {
    ctxt->clear();
  }
}


size_t km_kbp_context_length(km_kbp_context *ctxt)
{
  assert(ctxt);
  return ctxt ? ctxt->size() : 0;
}


km_kbp_status km_kbp_context_append(km_kbp_context *ctxt,
                                    km_kbp_context_item const *ci)
{
  assert(ctxt); assert(ci);
  if (!ctxt || !ci)   return KM_KBP_STATUS_INVALID_ARGUMENT;

  try
  {
    for (;ci->type != KM_KBP_CT_END; ++ci)
    {
      ctxt->emplace_back(*ci);
    }
  } catch(std::bad_alloc) {
    return KM_KBP_STATUS_NO_MEM;
  }

  return KM_KBP_STATUS_OK;
}


km_kbp_status km_kbp_context_shrink(km_kbp_context *ctxt, size_t num,
                           km_kbp_context_item const * ci)
{
  assert(ctxt);
  if (!ctxt)   return KM_KBP_STATUS_INVALID_ARGUMENT;

  try
  {
    ctxt->resize(ctxt->size() - std::min(num, ctxt->size()));

    if (ci)
    {
      auto const ip = ctxt->begin();
      while(num-- && ci->type != KM_KBP_CT_END)
      {
        ctxt->emplace(ip, *ci);
        ci++;
      }
    }
  } catch(std::bad_alloc) {
    return KM_KBP_STATUS_NO_MEM;
  }

  return KM_KBP_STATUS_OK;
}


json & operator << (json & j, km::kbp::context const & ctxt) {
  j << json::array;
  for (auto & i: ctxt)  j << i;
  return j << json::close;
}

json & operator << (json & j, km_kbp_context_item const & i)
{
  utf8::codeunit_t cps[5] = {0,};
  int8_t l;

  switch (i.type)
  {
    case KM_KBP_CT_CHAR:
      utf8::codec::put(cps, i.character, l);
      j << json::string(&cps[0]);
      break;
    case KM_KBP_CT_MARKER:
      j << json::integer_u(i.marker);
      break;
    default:
      j << json::flat << json::object
          << "invalid type" << i.type
          << json::close;
  }
  return j;
}
