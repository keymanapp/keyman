#ifndef _APPCONTEXT_H
#define _APPCONTEXT_H
/*
  Name:             appcontext
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      23 Nov 2023

  Modified Date:    23 Nov 2023
  Authors:          rcruickshank
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes: AppContext is retained to support calldll with the external interface for the 3rd party IMX keyboards
         that worked with KMX formatted Context Strings. It is also used once for debug logging the ProcessHook.
  History:
*/

class AppContext {
private:
  WCHAR CurContext[MAXCONTEXT];  //!< CurContext[0] is furthest from the caret and buffer is null terminated.
  int pos;

public:
  AppContext();

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
   * @param  bufsize  The number of code units ie size of the WCHAR buffer - not the code points
   */
  void Get(WCHAR *buf, int bufsize);

  /**
   * Sets the CurContext to the supplied buf character array and updates the pos index.
   *
   * @param buf
   */
  void Set(const WCHAR *buf);

  /**
   * Returns a pointer to the character in the current context buffer which
   * will have at most n valid xstring units remaining until the null terminating
   * character. It will be one code unit less than bufsize if that would
   * have meant splitting a surrogate pair or deadkey.
   *
   * @param n        The maximum number of valid xstring units (not code points or code units)
   * @return WCHAR*  Pointer to the start postion for a buffer of maximum n xstring units
   */
  WCHAR *BufMax(int n);

  /**
   * Returns TRUE if the last xstring unit in the context is a deadkey
   *
   * @return BOOL
   */
  BOOL CharIsDeadkey();

  /**
   * Returns TRUE if the last xstring unit in the CurContext is a surrogate pair.
   * @return BOOL
   */
  BOOL CharIsSurrogatePair();

  /**
   * Returns TRUE if the context is empty
   * @return  BOOL
   */
  BOOL AppContext::IsEmpty();
};

/**
 * Convert km_core_context_item array into an kmx char buffer.
 * Caller is responsible for freeing the memory.
 * The length is restricted to a maximum of MAXCONTEXT length. If the number
 * of input km_core_context_items exceeds this length the characters furthest
 * from the caret will be truncated.
 *
 * @param  contextItems  the input core context array. (km_core_context_item)
 * @param  [out] outBuf  the kmx character array output. caller to free memory.
 *
 * @return  BOOL    True if array created successfully
 */
BOOL ContextItemToAppContext(km_core_context_item *contextItems, PWSTR outBuf, DWORD len);



#endif
