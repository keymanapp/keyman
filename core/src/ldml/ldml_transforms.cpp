/*
  Copyright:    © SIL International.
  Description:  This is an implementation of the LDML keyboard spec 3.0.
  Create Date:  7 Oct 2022
  Authors:      Steven R. Loomis
*/

#include "ldml_transforms.hpp"
#include "ldml_markers.hpp"
#include "debuglog.h"
#include <algorithm>
#include <string>
#include "kmx/kmx_xstring.h"
#include <assert.h>
#include "ldml_utils.hpp"

namespace km {
namespace core {
namespace ldml {

/**
 * \def KMXPLUS_DEBUG_TRANSFORM
 * define KMXPLUS_DEBUG_TRANSFORM=1 to enable verbose processing of transforms/reorders
 * The default is 0, which only notes initialization and exceptional cases
*/

#ifndef KMXPLUS_DEBUG_TRANSFORM
#define KMXPLUS_DEBUG_TRANSFORM 0
#endif

#if KMXPLUS_DEBUG_TRANSFORM
#define DebugTran(msg, ...) DebugLog(msg, ##__VA_ARGS__)
#else
#define DebugTran(msg, ...)
#endif

element::element(const SimpleUSet &new_u, KMX_DWORD new_flags)
    : chr(), uset(new_u), flags((new_flags & ~LDML_ELEM_FLAGS_TYPE) | LDML_ELEM_FLAGS_TYPE_USET) {
}

element::element(km_core_usv ch, KMX_DWORD new_flags)
    : chr(ch), uset(), flags((new_flags & ~LDML_ELEM_FLAGS_TYPE) | LDML_ELEM_FLAGS_TYPE_CHAR) {
}

bool
element::is_uset() const {
  return (flags & LDML_ELEM_FLAGS_TYPE) == LDML_ELEM_FLAGS_TYPE_USET;
}

reorder_weight
element::get_order() const {
  unsigned char uorder = ((flags & LDML_ELEM_FLAGS_ORDER_MASK) >> LDML_ELEM_FLAGS_ORDER_BITSHIFT);
  return (reorder_weight)uorder; // unsigned to signed
}

reorder_weight
element::get_tertiary() const {
  unsigned char uorder = ((flags & LDML_ELEM_FLAGS_TERTIARY_MASK) >> LDML_ELEM_FLAGS_TERTIARY_BITSHIFT);
  return (reorder_weight)uorder; // unsigned to signed
}

bool
element::is_prebase() const {
  return !!(flags & LDML_ELEM_FLAGS_PREBASE);
}

bool
element::is_tertiary_base() const {
  return !!(flags & LDML_ELEM_FLAGS_TERTIARY_BASE);
}

KMX_DWORD
element::get_flags() const {
  return flags;
}

bool
element::matches(km_core_usv ch) const {
  if (is_uset()) {
    return uset.contains(ch);
  } else {
    return chr == ch;
  }
}

void
element::dump() const {
  if (is_uset()) {
    DebugLog("element order=%d USET", (int)get_order());
    uset.dump();
  } else {
    DebugLog("element order=%d U+%04X", (int)get_order(), (int)chr);
  }
}

int
reorder_sort_key::compare(const reorder_sort_key &other) const {
  auto primaryResult    = primary    - other.primary;
  auto secondaryResult  = secondary  - other.secondary;
  auto tertiaryResult   = tertiary   - other.tertiary;
  auto quaternaryResult = quaternary - other.quaternary;

  if (primaryResult) {
    return (int)primaryResult;
  } else if (secondaryResult) {
    return (int)secondaryResult;
  } else if (tertiaryResult) {
    return (int)tertiaryResult;
  } else if (quaternaryResult) {
    return (int)quaternaryResult;
  } else {
    // We don't expect to get here.  quaternaryResult is the string index, which
    // should be unequal.
    assert(quaternaryResult);
    // We have the underlying character, so use the binary order as a tiebreaker.
    int identityResult = (int)ch - (int)other.ch;  // tie breaker
    return identityResult;
  }
}

bool
reorder_sort_key::operator<(const reorder_sort_key &other) const {
  return (compare(other) < 0);
}

bool
reorder_sort_key::operator>(const reorder_sort_key &other) const {
  return (compare(other) > 0);
}

std::deque<reorder_sort_key>
reorder_sort_key::from(const std::u32string &str) {
  // construct a 'baseline' sort key, that is, in the absence of
  // any match rules.
  std::deque<reorder_sort_key> keylist;
  auto s           = str.begin();  // str iterator
  reorder_weight c = 0;            // str index
  for (auto e = str.begin(); e < str.end(); e++, s++, c++) {
    // primary    weight: 0
    // seconary   weight: c (the string index)
    // tertiary   weight: 0
    // quaternary weight: c (the index again)
    keylist.emplace_back(reorder_sort_key{*s, 0, c, 0, c, false});
  }
  return keylist;
}

void
reorder_sort_key::dump() const {
  // for debugging…
  DebugLog(
      "- U+%04X\t(%d, %d, %d, %d) %c", ch, (int)primary, (int)secondary, (int)tertiary, (int)quaternary,
      is_tertiary_base ? 'T' : ' ');
}

size_t
element_list::match_end(const std::u32string &str) const {
  if (str.length() < size()) {
    // input string too short, can't possibly match.
    // This assumes each element is a single char, no string elements.
    return 0;
  }
  // s: iterate from end to front of string
  // For example, if str = 'abcd', we try to match 'd', then 'c', then 'b', then 'a'
  // starting with the end of the element list.
  auto s = str.rbegin();
  // e: end to front on elements.
  // we know the # of elements is <= length of string,
  // so we don't need to check the string's boundaries
  for (auto e = rbegin(); e < rend(); e++) {
    assert(s < str.rend()); // double check
    if (!e->matches(*s)) {
      // break out if this element doesn't match
      return 0;
    }
    s++;
  }
  return size();  // match size = element size
}

bool
element_list::load(const kmx::kmx_plus &kplus, kmx::KMXPLUS_ELEM id) {
  KMX_DWORD elementsLength;
  auto elements = kplus.elem->getElementList(id, elementsLength); // pointer to beginning of element list
  assert((elementsLength == 0) || (elements != nullptr)); // it could be a 0-length list
  for (size_t i = 0; i<elementsLength; i++) {
    auto e = elements[i];
    KMX_DWORD flags = e.flags;
    auto type = flags & LDML_ELEM_FLAGS_TYPE;
    if (type == LDML_ELEM_FLAGS_TYPE_CHAR) {
      km_core_usv ch = e.element;
      emplace_back(ch, flags); // char
    } else if (type == LDML_ELEM_FLAGS_TYPE_USET) {
      // need to load a SimpleUSet
      auto u = kplus.usetHelper.getUset(e.element);
      if (!u.valid()) {
        DebugLog("Error, invalid UnicodeSet at element %d", (int)i);
        u.dump();
        assert(u.valid());
        return false;
      }
      emplace_back(u, flags);
    } else {
      // reorders don't use 'string' element types, so we don't expect them here.
      assert((type != LDML_ELEM_FLAGS_TYPE_USET) && (type != LDML_ELEM_FLAGS_TYPE_CHAR));
      return false;
    }
  }
#if KMXPLUS_DEBUG_TRANSFORM
  DebugTran("Loaded:");
  dump();
#endif
  return true;
}

std::deque<reorder_sort_key> &
element_list::update_sort_key(size_t offset, std::deque<reorder_sort_key> &key) const {
  /** string index */
  size_t c = 0;
  bool have_last_base                = false;
  reorder_weight last_base_primary   = -1;
  reorder_weight last_base_secondary = -1;
  for (auto e = begin(); e < end(); e++, c++) {
    /** position in the key */
    auto n = offset + c;
    /** update this key */
    auto &k = key.at(n);
    // we double check that the character matches. otherwise something
    // has really gone awry, because we shouldn't be here if this element list doesn't apply.
    if (!e->matches(k.ch)) {
      DebugLog("!! Internal Error: updateSortKey(%d+%d): element did not re-match the sortkey", offset, c);
      k.dump();
      // TODO-LDML: assertion follows
      assert(e->matches(k.ch));        // double check that this element matches
    }
    // we only update primary and tertiary weights
    k.primary          = e->get_order();
    k.tertiary         = e->get_tertiary();
    k.is_tertiary_base = e->is_tertiary_base();

    if (k.tertiary != 0 && n > 0) {
      // search backwards for a base
      auto n2 = n;
      // TODO-LDML: odd loop here because n2 is signed.
      do {
        n2--;
        auto &k2 = key.at(n2);
        if (k2.is_tertiary_base) {
          last_base_primary   = k2.primary;
          last_base_secondary = k2.secondary;
          have_last_base      = true;
        }
      } while (!have_last_base && n2 > 0);
      // we may not have found the base. but the common case is that the base is found.
      if (have_last_base) {
        // copy the primary and secondary from the last_base
        k.primary   = last_base_primary;
        k.secondary = last_base_secondary;
      }
    }
#if KMXPLUS_DEBUG_TRANSFORM
    DebugTran("Updating at +%d", c);
    k.dump();
#endif
  }
  return key;
}

void
element_list::dump() const {
  DebugLog("element_list[%d]", size());
  for (const auto &e : *this) {
    e.dump();
  }
}

reorder_entry::reorder_entry(const element_list &new_elements) : elements(new_elements), before() {
}
reorder_entry::reorder_entry(const element_list &new_elements, const element_list &new_before) : elements(new_elements), before(new_before) {
}

size_t
reorder_entry::match_end(std::u32string &str, size_t offset, size_t len) const {
  auto substr      = str.substr(offset, len);
  // first, see if the elements match. If not, this entry doesn't apply
  size_t match_len = elements.match_end(substr);
  if (match_len == 0) {
    return 0;
  }

  // Now we need to check if there is a "before=" element string that
  // is also a precondition.
  if (!before.empty()) {
    // does not match before offset
    std::u32string prefix = substr.substr(0, substr.length() - match_len);
    // make sure the 'before' is present
    if (before.match_end(prefix) == 0) {
      return 0;  // break out.
    }
  }
  return match_len;
}

int
reorder_entry::compare(const reorder_entry &other) const {
  if (this == &other) {
    return 0;
  } else if (elements.size() < other.elements.size()) {
    return -1;
  } else if (elements.size() > other.elements.size()) {
    return 1;
  } else {
    return 0; // punt
  }
}

bool
reorder_entry::operator<(const reorder_entry &other) const {
  return (compare(other) < 0);
}

bool
reorder_entry::operator>(const reorder_entry &other) const {
  return (compare(other) > 0);
}

bool
reorder_group::apply(std::u32string &str) const {
  /** did we apply anything */
  bool applied = false;
  /** did we match anything */
  bool some_match = false;

  // markers need to 'pass through' reorders. remove and re-add if needed
  marker_map markers;
  std::u32string out = remove_markers(str, markers, plain_sentinel);

  // get a baseline sort key
  auto sort_keys = reorder_sort_key::from(out);

  // apply ALL reorders in the group.
  for (const auto &r : list) {
    // work backward from end of string forward
    // That is, see if "abc" matches "abc" or "ab" or "a"
    for (size_t s = out.length(); s > 0; s--) {
      size_t submatch = r.match_end(out, 0, s);
      if (submatch != 0) {
#if KMXPLUS_DEBUG_TRANSFORM
        DebugTran("Matched: %S (off=%d, len=%d)", str.c_str(), 0, s);
        r.elements.dump();
#endif
        // update the sort key
        size_t sub_match_start = s - submatch;
        r.elements.update_sort_key(sub_match_start, sort_keys);
        some_match = true; // record that there was a match
      }
    }
  }
  if (!some_match) {
    // get out if nothing matched.
    // the sortkey won't be "interesting", and the sort
    // will be a no-op.
    DebugTran("Skip: No reorder elements matched.");
    return false;  // nothing matched, so no work.
  }

#if KMXPLUS_DEBUG_TRANSFORM
  DebugTran("Updated sortkey");
  for (const auto &r : sort_keys) {
    r.dump();
  }
#endif

  // Now, we need to actually do the sorting, but we must only sort
  // 'runs' beginning with 0-weight keys.

  // Consider the 'roast' example in the spec, you might end up with the following:
  //    codepoint  (pri, sec, ter, quat)
  //  U+1A21	(0, 0, 0, 0)
  //  U+1A60	(127, 1, 0, 1)
  //  U+1A45	(0, 2, 0, 2)
  //  U+1A6B	(42, 3, 0, 3)
  //  U+1A76	(55, 4, 0, 4)
  // This example happens to be in order, but must be sorted in two diferent ranges,
  // with secondary (index) values of [0,1] and [2,4]
  //
  // Another example might look like the following:
  //  U+1A21	(0, 0, 0, 0)
  //  U+1A6B	(42, 1, 0, 1)
  //  U+1A76	(55, 2, 0, 2)
  //  U+1A60	(10, 3, 0, 3)
  //  U+1A45	(10, 4, 0, 3)
  // Here there is only a single range to sort [0,4]

  /** pointer to the beginning of the current run. */
  std::deque<reorder_sort_key>::iterator run_start = sort_keys.begin();
  for(auto e = run_start; e != sort_keys.end(); e++) {
    // find the actual beginning base: primary weight = 0 and tertiary = 0.
    // (tertiary chars will have primary=0 BUT will have tertiary nonzero.)
    if ((e->primary == 0) && (e->tertiary == 0) && (e != run_start)) {
      auto run_end = e - 1;
      DebugTran("Sorting subrange quaternary=[%d..]", run_start->quaternary);
      std::sort(run_start, run_end);  // reversed because it's a reverse iterator…?
      // move the start
      run_start = e; // next run starts here
    }
  }
  // sort the last run in the string as well.
  if (run_start != sort_keys.end()) { // TODO-LDML: skip if a single-char run
    DebugTran("Sorting final subrange quaternary=[%d..]", run_start->quaternary);
    std::sort(run_start, sort_keys.end()); // reversed because it's a reverse iterator…?
  }
  // recombine into a string by pulling out the 'ch' value
  // that's in each sortkey element.
  out.clear(); // will re-add all text
  signed char q = sort_keys.begin()->quaternary; // start with the first quaternary
  for (auto e = sort_keys.begin(); e < sort_keys.end(); e++, q++) {
    if (q != e->quaternary) {
      // something rearranged in this subrange, because the quaternary values are out of order.
      applied = true;
    }
    // collect the characters
    out.append(1, e->ch);
  }
  if (!applied) {
    DebugTran("Skip: sorting caused no reordering");
    // exit early to avoid string copying and possibly marker re-adding.
    return false; // no change
  }
#if KMXPLUS_DEBUG_TRANSFORM
  DebugTran("Sorted sortkey");
  for (const auto &r : sort_keys) {
    r.dump();
  }
#endif
  add_back_markers(str, out, markers, plain_sentinel);
  return true; // updated
}

transform_entry::transform_entry(const transform_entry &other)
    : fFrom(other.fFrom), fTo(other.fTo), fFromPattern(nullptr), fMapFromStrId(other.fMapFromStrId),
      fMapToStrId(other.fMapToStrId), fMapFromList(other.fMapFromList), fMapToList(other.fMapToList),
      normalization_disabled(other.normalization_disabled) {
  if (other.fFromPattern) {
    // clone pattern
    fFromPattern.reset(other.fFromPattern->clone());
  }
}

transform_entry::transform_entry(const std::u32string &from, const std::u32string &to)
    : fFrom(from), fTo(to), fFromPattern(nullptr), fMapFromStrId(), fMapToStrId(), fMapFromList(), fMapToList(), normalization_disabled(false) {
  assert(!fFrom.empty());

  init();
}

transform_entry::transform_entry(
    const std::u32string &from,
    const std::u32string &to,
    KMX_DWORD mapFrom,
    KMX_DWORD mapTo,
    const kmx::kmx_plus &kplus,
    bool &valid,
    bool norm_disabled)
    : fFrom(from), fTo(to), fFromPattern(nullptr), fMapFromStrId(mapFrom), fMapToStrId(mapTo), normalization_disabled(norm_disabled) {
  if (!valid)
    return; // exit early
  assert(!fFrom.empty()); // TODO-LDML: should not happen?
  assert((fMapFromStrId == 0) == (fMapToStrId == 0));  // we have both or we have neither.
  assert(kplus.strs != nullptr);
  assert(kplus.vars != nullptr);
  assert(kplus.elem != nullptr);
  if(!init()) {
    valid = false;
  }

  // setup mapFrom
  if (fMapFromStrId != 0) {
    // Note: if we need the variable name it is available as follows,
    // but isn't needed for normal processing. Could be useful for debug messages.
    //  auto mapFrom = kplus.strs->get(fMapFromStrId);
    //  auto mapTo   = kplus.strs->get(fMapToStrId);

    // get the vars
    auto *fromVar = kplus.vars->findByStringId(fMapFromStrId);
    auto *toVar   = kplus.vars->findByStringId(fMapToStrId);
    assert(fromVar != nullptr);
    assert(toVar   != nullptr);


    // get the element lists
    assert(fromVar->type == LDML_VARS_ENTRY_TYPE_SET);
    assert(toVar->type   == LDML_VARS_ENTRY_TYPE_SET);
    KMX_DWORD fromLength, toLength;
    auto *fromList = kplus.elem->getElementList(fromVar->elem, fromLength);
    auto *toList   = kplus.elem->getElementList(toVar->elem, toLength);
    assert(fromLength == toLength);
    assert(fromList != nullptr);
    assert(toList   != nullptr);

    // populate the deques from the lists
    fMapFromList = fromList->loadAsStringList(fromLength, *(kplus.strs));
    fMapToList   = toList->loadAsStringList(toLength, *(kplus.strs));
    // did we get the expected items?
    assert(fMapFromList.size() == fromLength);
    assert(fMapToList.size()   == toLength);
  }
}

bool
transform_entry::init() {
  if (fFrom.empty()) {
    return false;
  }
  // TODO-LDML: if we have mapFrom, may need to do other processing.
  std::u32string from2 = fFrom;
  if (!normalization_disabled) {
    // normalize, including markers, for regex
    normalize_nfd_markers(from2, regex_sentinel);
  }
  std::u16string patstr = km::core::kmx::u32string_to_u16string(from2);
  UErrorCode status           = U_ZERO_ERROR;
  /* const */ icu::UnicodeString patustr = icu::UnicodeString(patstr.data(), (int32_t)patstr.length());
  // add '$' to match to end
  patustr.append(u'$'); // TODO-LDML: may need to escape some markers. Marker #91 will look like a `[` to the pattern
  fFromPattern.reset(icu::RegexPattern::compile(patustr, 0, status));
  return (UASSERT_SUCCESS(status));
}

size_t
transform_entry::apply(const std::u32string &input, std::u32string &output) const {
  assert(fFromPattern);
  // TODO-LDML: Really? can't go from u32 to UnicodeString?
  // TODO-LDML: Also, we could cache the u16 string at the transformGroup level or higher.
  UErrorCode status = U_ZERO_ERROR;
  const std::u16string matchstr = km::core::kmx::u32string_to_u16string(input);
  icu::UnicodeString matchustr  = icu::UnicodeString(matchstr.data(), (int32_t)matchstr.length());
  // TODO-LDML: create a new Matcher every time. These could be cached and reset.
  std::unique_ptr<icu::RegexMatcher> matcher(fFromPattern->matcher(matchustr, status));
  if (!UASSERT_SUCCESS(status)) {
    return 0; // TODO-LDML: return error
  }

  if (!matcher->find(status)) { // i.e. matches somewhere, in this case at end of str
    return 0; // no match
  }

  // TODO-LDML: this is UTF-16 len, not UTF-32 len!!
  // TODO-LDML: if we had an underlying UText this would be simpler.
  int32_t matchStart = matcher->start(status);
  int32_t matchEnd   = matcher->end(status);
  if (!UASSERT_SUCCESS(status)) {
    return 0; // TODO-LDML: return error
  }
  // extract..
  const icu::UnicodeString substr = matchustr.tempSubStringBetween(matchStart, matchEnd);
  // preflight to UTF-32 to get length
  UErrorCode substrStatus = U_ZERO_ERROR; // throwaway status
  // we need the UTF-32 matchLen for our return.
  auto matchLen = substr.toUTF32(nullptr, 0, substrStatus);

  // should have matched something.
  assert(matchLen > 0);

  // now, do the replace.

  /** this is the 'to' or other replacement string.*/
  icu::UnicodeString rustr;
  if (fMapFromStrId == 0) {
    // Normal case: not a map.
    // This replace will apply $1, $2 etc.
    // Convert the fTo into u16 TODO-LDML (we could cache this?)
    const std::u16string rstr = km::core::kmx::u32string_to_u16string(fTo);
    rustr  = icu::UnicodeString(rstr.data(), (int32_t)rstr.length());
  } else {
    // Set map case: mapping from/to

    // we actually need the group(1) string here.
    // this is only the content in parenthesis ()
    icu::UnicodeString group1 = matcher->group(1, status);
    if (!UASSERT_SUCCESS(status)) {
      // TODO-LDML: could be a malformed from pattern
      return 0; // TODO-LDML: return error
    }
    // now, how long is group1 in UTF-32, hmm?
    UErrorCode preflightStatus = U_ZERO_ERROR; // throwaway status
    auto group1Len             = group1.toUTF32(nullptr, 0, preflightStatus);
    char32_t *s                = new char32_t[group1Len + 1];
    assert(s != nullptr); // TODO-LDML: OOM
    // convert
    group1.toUTF32((UChar32 *)s, group1Len + 1, status);
    if (!UASSERT_SUCCESS(status)) {
      return 0; // TODO-LDML: memory issue
    }
    std::u32string match32(s, group1Len); // taken from just group1
    // clean up buffer
    delete [] s;

    // Now we're ready to do the actual mapping.

    // 1., we need to find the index in the source set.
    auto matchIndex = findIndexFrom(match32);
    assert(matchIndex != -1L); // TODO-LDML: not matching shouldn't happen, the regex wouldn't have matched.
    // we already asserted on load that the from and to sets have the same cardinality.

    // 2. get the target string, convert to utf-16
    // we use the same matchIndex that was just found
    const std::u16string rstr = km::core::kmx::u32string_to_u16string(fMapToList.at(matchIndex));

    // 3. update the UnicodeString for replacement
    rustr  = icu::UnicodeString(rstr.data(), (int32_t)rstr.length());
    // and we return to the regular code flow.
  }
  // here we replace the match output. No normalization, yet.
  icu::UnicodeString entireOutput = matcher->replaceFirst(rustr, status);
  if (!UASSERT_SUCCESS(status)) {
    // TODO-LDML: could fail here due to bad input (syntax err)
    return 0;
  }
  // entireOutput includes all of 'input', but modified. Need to substring it.
  icu::UnicodeString outu = entireOutput.tempSubString(matchStart);

  // Special case if there's no output, save some allocs
  if (outu.length() == 0) {
    output.clear();
  } else {
    // TODO-LDML: All we are trying to do is to extract the output string. Probably too many steps.
    UErrorCode preflightStatus = U_ZERO_ERROR;
    // calculate how big the buffer is
    auto out32len              = outu.toUTF32(nullptr, 0, preflightStatus); // preflightStatus will be an err, because we know the buffer overruns zero bytes
    // allocate
    std::unique_ptr<char32_t[]> s(new char32_t[out32len + 1]);
    assert(s);
    if (!s) {
      return 0; // TODO-LDML: allocation failed
    }
    // convert
    outu.toUTF32((UChar32 *)(s.get()), out32len + 1, status);
    if (!UASSERT_SUCCESS(status)) {
      return 0; // TODO-LDML: memory issue
    }
    output.assign(s.get(), out32len);
    // NOW do a marker-safe normalize
    if (!normalization_disabled && !normalize_nfd_markers(output)) {
      DebugLog("normalize_nfd_markers(output) failed");
      return 0; // TODO-LDML: normalization failed.
    }
  }
  return matchLen;
}

int32_t transform_entry::findIndexFrom(const std::u32string &match) const {
  return findIndex(match, fMapFromList);
}

int32_t transform_entry::findIndex(const std::u32string &match, const std::deque<std::u32string> list) {
  int32_t index = 0;
  for(auto e = list.begin(); e < list.end(); e++, index++) {
    if (match == *e) {
      return index;
    }
  }
  return -1; // not found
}

any_group::any_group(const transform_group &g) : type(any_group_type::transform), transform(g), reorder() {
}

any_group::any_group(const reorder_group &g) : type(any_group_type::reorder), transform(), reorder(g) {
}

size_t
any_group::apply(std::u32string &input, std::u32string &output, size_t matched) const  {
  if (type == any_group_type::transform) {
    return apply_transform(input, output, matched);
  } else if(type == any_group_type::reorder) {
    return apply_reorder(input, output, matched);
  } else {
    assert(type != any_group_type::transform && type != any_group_type::reorder);
    return matched;
  }
}

size_t
any_group::apply_transform(std::u32string &input, std::u32string &output, size_t matched) const {
  std::u32string subOutput;
  size_t subMatched = transform.apply(input, subOutput);

  if (subMatched == 0) {
    return matched; // no match, break out
  }

  // remove the matched part of the input
  assert(subMatched <= input.length());
  input.resize(input.length() - subMatched);  // chop off the subMatched part at end
  input.append(subOutput);                    // subOutput could be empty such as in backspace transform

  if (subMatched <= output.length()) {
    // remove matched part of output
    output.resize(output.length() - subMatched);
  } else {
    // matched past beginning of 'output', expand match
    matched += (subMatched - output.length());
    output.resize(0);
  }
  output.append(subOutput);

  return matched;
}

size_t
any_group::apply_reorder(std::u32string &input, std::u32string &output, size_t matched) const {
  std::u32string str2 = input;
  if (reorder.apply(str2)) {
    output.resize(0);
    output.append(str2);
    input.resize(0);
    input.append(str2);
    // Consider the entire string as 'matched'.
    // The calling chain will determine which characters actually changed.
    matched = output.length();
  }
  return matched;
}

transforms::transforms(bool norm_disabled) : transform_groups(), normalization_disabled(norm_disabled) {
}

void
transforms::addGroup(const transform_group &s) {
  transform_groups.emplace_back(s);
}

void
transforms::addGroup(const reorder_group &s) {
  transform_groups.emplace_back(s);
}

transform_group::transform_group() {
}

/**
 * return the first transform match in this group
 */
size_t
transform_group::apply(const std::u32string &input, std::u32string &output) const {
  size_t subMatched = 0;
  for (auto transform = begin(); (subMatched == 0) && (transform < end()); transform++) {
    // TODO-LDML: non regex implementation
    // is the match area too short?
    subMatched = transform->apply(input, output);
    if (subMatched != 0) {
      return subMatched; // matched. break out.
    }
  }
  return 0; // no match
}

/**
 * Apply this entire transform set to the input.
 * Example: input "abc" -> output="xyz", return=2:  replace last two chars "bc" with "xyz", so final output = "abxyz";
 * @param input input string, will match at end: unmodified
 * @param output cleared. on output: if return>0, contains text to replace
 * @return match length: number of chars at end of input string to modify.  0 if no match.
 */
size_t
transforms::apply(const std::u32string &input, std::u32string &output) {
  /**
   * Example:
   * Group0:   za -> c, a -> bb
   * Group1:   bb -> ccc
   * Group2:   cc -> d
   * Group3:   tcd -> e
   *
   * Initial condition: matched = subMatched = 0. updatedInput = input = 'ta', output = ''
   *
   * Group0:  matches 'a',  subMatched=1, output='bb', updatedInput='tbb', matched=1
   *  Initial match. subMatched=1 which is > the previous output.length,
   *  so matched += (1-0) == 1
   *
   * Group1:  matches 'bb', subMatched=2, output='ccc', updatedInput='tccc', matched=1
   *  Although the transform group matched 2 chars ('bb'), the match in the original is 1
   *  This time subMatched=2 but previous output.length was 2, so matched remains at 1
   *  In other words, we didn't match outside of the existing match boundary.
   *
   * Group2:  matches 'cc', subMatched=2, output='cd', updatedInput='tcd', matched=1
   *  subMatched <= previous output.length, so no change to mathed
   *
   * Group3:  matches 'tcd', subMatched=3, output='e', updatedInput='e', matched=2
   *  Now subMatched=3, but previous output ('cd').length is only 2.
   *  In other words, we matched before the previous output's start - we matched the 't' also
   *  or this reason, matched+=(3-2) == 1.  So matched is now 2.
   */

  /**
   * Accumulate the maximum matched size of the end of 'input' here.
   * It will be replaced with 'output'.
   * Matched can increment.
   */
  size_t matched = 0;
  output.clear();
  /** modified copy of input, to pass to each next step */
  std::u32string updatedInput = input;

  // loop over each group of transforms
  for (auto group = transform_groups.begin(); group < transform_groups.end(); group++) {
    matched = group->apply(updatedInput, output, matched);
  }

  /**
   * Could optimize to contract 'matched' if possible.
   * We could decrement 'matched' for every char of output
   * which is already in input. Example (regex example):
   * - str = "xxyyzz";
   * - s/zz$/z/ -> match=2, output='z'
   * - s/z$/zz/ -> match=2, output='zz'
   *      (but could contract to match=0, output='' as already present)
   * - s/zz$/zw/ -> match=2, output='zw'
   *      (but could contract to match=1, output='w')
   *
   * could also handle from="x" to="x" as match=0
   *
   * However, the calling code already checks for common prefixes,
   * so this does not need to be optimized.
   */
  return matched;
}

bool
transforms::apply(std::u32string &str) {
  // simple implementation for tests
  std::u32string output;
  size_t matchLength = apply(str, output);
  if (matchLength == 0) {
    return false;
  }
  str.resize(str.size() - matchLength);
  str.append(output);
  return true;
}

transforms *
transforms::load(
    const kmx::kmx_plus &kplus,
    const core::kmx::COMP_KMXPLUS_TRAN *tran,
    const core::kmx::COMP_KMXPLUS_TRAN_Helper &tranHelper) {
  bool valid = true;
  if (tran == nullptr) {
    DebugLog("for tran: tran is null");
    valid = false;
  } else if (!tranHelper.valid()) {
    DebugLog("for tran: tranHelper is invalid");
    valid = false;
  } else if (nullptr == kplus.elem) {
    DebugLog("for tran: kplus.elem == nullptr");
    valid = false;
  } else if (nullptr == kplus.strs) {
    DebugLog("for tran: kplus.strs == nullptr");  // need a string table to get strings
    valid = false;
  } else if (nullptr == kplus.vars) {
    DebugLog("for tran: kplus.vars == nullptr");  // need a vars table to get maps
    valid = false;
  } else if (nullptr == kplus.meta) {
    DebugLog("for tran: kplus.meta == nullptr");  // need a meta table to check normalization
    valid = false;
  }

  assert(valid);
  if (!valid) {
    return nullptr;
  }

  // with that out of the way, let's set it up

  std::unique_ptr<transforms> transforms;

  const bool normalization_disabled = kplus.meta->normalization_disabled();

  transforms.reset(new ldml::transforms(normalization_disabled));

  for (KMX_DWORD groupNumber = 0; groupNumber < tran->groupCount; groupNumber++) {
    const kmx::COMP_KMXPLUS_TRAN_GROUP *group = tranHelper.getGroup(groupNumber);
    // C++20 returns a reference here
    // auto &newGroup = allGroups->emplace_back();

    if (group->type == LDML_TRAN_GROUP_TYPE_TRANSFORM) {
      transform_group newGroup;

      for (KMX_DWORD itemNumber = 0; itemNumber < group->count; itemNumber++) {
        const kmx::COMP_KMXPLUS_TRAN_TRANSFORM *element = tranHelper.getTransform(group->index + itemNumber);
        const std::u32string fromStr                    = kmx::u16string_to_u32string(kplus.strs->get(element->from));
        const std::u32string toStr                      = kmx::u16string_to_u32string(kplus.strs->get(element->to));
        KMX_DWORD mapFrom                               = element->mapFrom; // copy, because of alignment
        KMX_DWORD mapTo                                 = element->mapTo;   // copy, because of alignment
        assert(!fromStr.empty());
        if (fromStr.empty()) {
          valid = false;
        }
        newGroup.emplace_back(fromStr, toStr, mapFrom, mapTo, kplus, valid, transforms->normalization_disabled);  // creating a transform_entry
        assert(valid);
        if(!valid) {
          return nullptr;
        }
      }
      transforms->addGroup(newGroup);
    } else if (group->type == LDML_TRAN_GROUP_TYPE_REORDER) {
      reorder_group newGroup;

      // fetch each reorder in the group
      for (KMX_DWORD itemNumber = 0; itemNumber < group->count; itemNumber++) {
        const kmx::COMP_KMXPLUS_TRAN_REORDER *reorder = tranHelper.getReorder(group->index + itemNumber);

        element_list elements;
        element_list before;

        bool load_ok = elements.load(kplus, reorder->elements) && before.load(kplus, reorder->before);
        assert(load_ok);
        if (load_ok) {
          newGroup.list.emplace_back(elements, before);
        } else {
          DebugLog("reorder elements(%d+%d) failed to load", group->index, itemNumber);
          return nullptr;
        }
      }
      std::sort(newGroup.list.begin(), newGroup.list.end()); // sort list by size, so that longer matches match last
      transforms->addGroup(newGroup);
    } else {
      // internal error - some other type - should have been caught by validation
      DebugLog("ERROR: some other type");
      assert(false);
      return nullptr;
    }
  }
  assert(valid);
  if (!valid) {
    return nullptr;
  } else {
    return transforms.release();
  }
}

}  // namespace ldml
}  // namespace core
}  // namespace km
