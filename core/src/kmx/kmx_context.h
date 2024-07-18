/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/

#pragma once

#include "kmx_base.h"

#define MAXCONTEXT 64

namespace km {
namespace core {
namespace kmx {

class KMX_Context
{
private:
  KMX_WCHAR CurContext[MAXCONTEXT];
  int pos;

public:
  KMX_Context();

  /**
   * Copy "source" AppContext to this AppContext
   *
   * @param  source  KMX_Context to copy
   */
  void CopyFrom(KMX_Context *source);

  /**
   * Add a single code unit to the Current Context. Not necessarily a complete code point
   *
   * @param  ch  Code unit to add
   */
  void Add(KMX_WCHAR ch);

  /**
  * Removes a single code point from the end of the CurContext closest to the caret;
  * i.e. it will be both code units if a surrogate pair. If it is a deadkey it will
  * remove three code points: UC_SENTINEL, CODE_DEADKEY and deadkey value.
  */
  void Delete();

   /**
   * Clears the CurContext and resets the position - pos - index
   */
  void Reset();

   /**
   * Copies the characters in CurContext to supplied buffer.
   * If bufsize is reached before the entire context was copied, the buf
   * will be truncated to number of valid characters possible with null character
   * termination. e.g. it will be one code unit less than bufsize if that would
   * have meant splitting a surrogate pair
   * @param  buf      The data buffer to copy current context
   * @param  bufsize  The number of code units ie size of the KMX_WCHAR buffer - not the code points
   */
  void Get(KMX_WCHAR *buf, int bufsize);

   /**
   *  Sets the CurContext to the supplied buf character array and updates the pos index.
   *
   * @param buf
   */
  void Set(const KMX_WCHAR *buf);

  /**
   * Returns a pointer to the character in the current context buffer which
   * will have at most n valid xstring units remaining until the null terminating
   * character. It will be one code unit less than bufsize if that would
   * have meant splitting a surrogate pair or deadkey.
   *
   * @param  n            The maximum number of valid xstring units (not code points or code units)
   * @return  KMX_WCHAR*  Pointer to the start postion for a buffer of maximum n xstring units
   */
  KMX_WCHAR *BufMax(int n);

  /**
   * Returns a pointer to the character in the current context buffer which
   * will have n valid xstring units remaining until the the null terminating character.
   * OR
   * Returns NULL if there are less than n valid xstring units in the current context.
   * Background this was historically for performance during rule evaluation, if there
   * are not enough characters to compare, don't event attempt the comparison.
   *
   * @param n  The number of valid xstring units (not code points or code units)
   * @return KMX_WCHAR* Pointer to the start postion for a buffer of maximum n characters
   */
  KMX_WCHAR *Buf(int n);

  /**
   * Return a pointer to the CurContext buffer
   *
   * @return KMX_WCHAR*
   */
  const KMX_WCHAR *GetFullContext() { return CurContext; }

  /**
   * Returns TRUE if the last xstring unit in the context is a deadkey
   *
   * @return BOOL
   */
  KMX_BOOL CharIsDeadkey();

  /**
  * Returns TRUE if the last xstring unit in the CurContext is a surrogate pair.
  * @return BOOL
  */
  KMX_BOOL CharIsSurrogatePair();

  /**
   * Returns TRUE if the context is empty
   * @return  BOOL
   */
  KMX_BOOL IsEmpty();
};

} // namespace kmx
} // namespace core
} // namespace km
