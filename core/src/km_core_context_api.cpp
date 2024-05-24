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

#include "keyman_core.h"

#include "context.hpp"
#include "jsonpp.hpp"
#include "utfcodec.hpp"

namespace {
  template<class U>
  km_core_status
  _context_items_from(typename U::codeunit_t const *text,
                      km_core_context_item **out_ptr)
  {
    assert(text); assert(out_ptr);
    if (!text || !out_ptr)  return KM_CORE_STATUS_INVALID_ARGUMENT;

    *out_ptr = nullptr;
    try
    {
      std::vector<km_core_context_item> res;

      for (auto i = typename U::const_iterator(text); *i; ++i)
      {
        if(i.error())   return KM_CORE_STATUS_INVALID_UTF;
        res.emplace_back(km_core_context_item {KM_CORE_CT_CHAR, {0,}, {*i}});
      }
      // Terminate the context_items array.
      res.emplace_back(km_core_context_item KM_CORE_CONTEXT_ITEM_END);

      *out_ptr = new km_core_context_item[res.size()];
      std::move(res.begin(), res.end(), *out_ptr);
    }
    catch (std::bad_alloc &)
    {
      return KM_CORE_STATUS_NO_MEM;
    }

    return KM_CORE_STATUS_OK;
  }

  template<class U>
  km_core_status _context_items_to(km_core_context_item const *ci,
                                  typename U::codeunit_t *buf,
                                  size_t * sz_ptr)
  {
    assert(ci); assert(sz_ptr);
    if (!ci || !sz_ptr)   return KM_CORE_STATUS_INVALID_ARGUMENT;
    auto const buf_size = *sz_ptr;

    if (buf && buf_size > 0)
    {
      auto i = typename U::iterator(buf);
      auto const e = decltype(i)(buf + buf_size - 1);

      for (;i != e && ci->type != KM_CORE_CT_END && !i.error(); ++ci)
      {
        if (ci->type == KM_CORE_CT_CHAR)
        {
          *i = ci->character; ++i;
        }
      }
      *i = 0; // null terminate the UTF16 string.

      *sz_ptr = buf_size - (e - i);

      // Skip over any final markers - they are execluded from context
      while(ci->type == KM_CORE_CT_MARKER) {
        ci++;
      }

      return ci->type == KM_CORE_CT_END
              ? KM_CORE_STATUS_OK
              : KM_CORE_STATUS_INSUFFICENT_BUFFER;
    }
    else
    {
      auto n = 0;
      typename U::codeunit_t cps[4];

      do
      {
        if (ci->type == KM_CORE_CT_CHAR)
        {
          int8_t l = 4;
          U::codec::put(cps, ci->character, l);
          n += l;
        }
      }
      while(ci++->type != KM_CORE_CT_END);

      *sz_ptr = n+1;
      return KM_CORE_STATUS_OK;
    }
  }
}

km_core_status
context_items_from_utf16(km_core_cu const *text,
                                km_core_context_item **out_ptr)
{
  return _context_items_from<utf16>(reinterpret_cast<utf16::codeunit_t const *>(text), out_ptr);
}


km_core_status context_items_to_utf8(km_core_context_item const *ci,
                                            char *buf, size_t * sz_ptr)
{
  return _context_items_to<utf8>(ci,
            reinterpret_cast<utf8::codeunit_t *>(buf),
            sz_ptr);
}


km_core_status context_items_to_utf16(km_core_context_item const *ci,
                                            km_core_cu *buf, size_t * sz_ptr)
{
  return _context_items_to<utf16>(ci,
            reinterpret_cast<utf16::codeunit_t *>(buf),
            sz_ptr);
}

km_core_status context_items_to_utf32(km_core_context_item const *ci,
                                            km_core_usv *buf, size_t * sz_ptr)
{
  return _context_items_to<utf32>(ci,
            reinterpret_cast<utf32::codeunit_t *>(buf),
            sz_ptr);
}

void km_core_context_items_dispose(km_core_context_item *ci)
{
  delete [] ci;
}


km_core_status km_core_context_set(km_core_context *ctxt, km_core_context_item const *ci)
{
    km_core_context_clear(ctxt);
    return context_append(ctxt, ci);
}


km_core_status km_core_context_get(km_core_context const *ctxt,
                                 km_core_context_item **out_ptr)
{
  assert(ctxt); assert(out_ptr);
  if (!ctxt || !out_ptr)   return KM_CORE_STATUS_INVALID_ARGUMENT;

  try
  {
    *out_ptr = new km_core_context_item[ctxt->size() + 1];
  }
  catch (std::bad_alloc &)
  {
    return KM_CORE_STATUS_NO_MEM;
  }
  std::copy(ctxt->begin(), ctxt->end(), *out_ptr);
  (*out_ptr)[ctxt->size()].type = KM_CORE_CT_END;

  return KM_CORE_STATUS_OK;
}


void km_core_context_clear(km_core_context *ctxt)
{
  assert(ctxt);
  if (ctxt)
  {
    ctxt->clear();
  }
}


size_t km_core_context_length(km_core_context *ctxt)
{
  assert(ctxt);
  return ctxt ? ctxt->size() : 0;
}


km_core_status context_append(km_core_context *ctxt,
                              km_core_context_item const *ci)
{
  assert(ctxt); assert(ci);
  if (!ctxt || !ci)   return KM_CORE_STATUS_INVALID_ARGUMENT;

  try
  {
    for (;ci->type != KM_CORE_CT_END; ++ci)
    {
      assert(ci->type == KM_CORE_CT_CHAR || ci->type == KM_CORE_CT_MARKER);
      if(ci->type == KM_CORE_CT_CHAR || ci->type == KM_CORE_CT_MARKER) {
        assert(ctxt->has_markers || ci->type != KM_CORE_CT_MARKER);
        if(ctxt->has_markers || ci->type != KM_CORE_CT_MARKER) {
          ctxt->emplace_back(*ci);
        }
      }
    }
  } catch (std::bad_alloc &) {
    return KM_CORE_STATUS_NO_MEM;
  }

  return KM_CORE_STATUS_OK;
}

km_core_status
context_prepend(
  km_core_context *ctxt,
  km_core_context_item const *ci,
  size_t num
) {
  assert(ctxt);
  assert(ci);
  if (!ctxt || !ci)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  try {
    std::vector<km_core_context_item> vci;
    for (; ci->type != KM_CORE_CT_END && num > 0; ++ci) {
      num--;
      assert(ci->type == KM_CORE_CT_CHAR || ci->type == KM_CORE_CT_MARKER);
      if(ci->type == KM_CORE_CT_CHAR || ci->type == KM_CORE_CT_MARKER) {
        assert(ctxt->has_markers || ci->type != KM_CORE_CT_MARKER);
        if(ctxt->has_markers || ci->type != KM_CORE_CT_MARKER) {
          vci.push_back(*ci);
        }
      }
    }

    ctxt->insert(ctxt->begin(), vci.begin(), vci.end());
  } catch (std::bad_alloc &) {
    return KM_CORE_STATUS_NO_MEM;
  }

  return KM_CORE_STATUS_OK;
}

km_core_status
context_shrink(
  km_core_context *ctxt,
  size_t num,
  bool from_end
) {
  assert(ctxt);
  if (!ctxt)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  if (from_end) {
    // remove from the end
    try {
      ctxt->resize(ctxt->size() - std::min(num, ctxt->size()));
    } catch (std::bad_alloc &) {
      return KM_CORE_STATUS_NO_MEM;
    }

    return KM_CORE_STATUS_OK;
  }

  // remove from the beginning
  auto it = ctxt->begin();
  while (num > 0 && it != ctxt->end()) {
    num--;
    it++;
  }

  ctxt->erase(ctxt->begin(), it);
  return KM_CORE_STATUS_OK;
}

size_t
km_core_context_item_list_size(km_core_context_item const *context_items)
{
  assert(context_items);
  if (!context_items)  return 0;

  auto n = 0;
  for (; context_items->type; ++context_items) {
    ++n;
  }

  return n;
}

json & operator << (json & j, km::core::context const & ctxt) {
  j << json::array;
  for (auto & i: ctxt)  j << i;
  return j << json::close;
}

json & operator << (json & j, km_core_context_item const & i)
{
  utf8::codeunit_t cps[7] = {0,}; // 6 bytes for maximal UTF-8 char (e.g. U+10FFFF) + nul terminator
  int8_t l = 4;

  switch (i.type)
  {
    case KM_CORE_CT_CHAR:
      utf8::codec::put(cps, i.character, l);
      j << json::string(&cps[0]);
      break;
    case KM_CORE_CT_MARKER:
      j << json::integer_u(i.marker);
      break;
    default:
      j << json::flat << json::object
          << "invalid type" << i.type
          << json::close;
  }
  return j;
}
