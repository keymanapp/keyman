/*
  Name:             appint
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    23 Feb 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Remove NoSetShift flag
                    27 Jan 2009 - mcdurdin - I1797 - Add fallback for AIWin2000 app integration
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    24 Jun 2010 - mcdurdin - I2436 - Add space to context for AIWin2000Unicode when not matched
                    24 Apr 2014 - mcdurdin - I4196 - V9.0 - wm_kmmoreposting must be refactored for TIP work as it is not sequential
                    23 Jun 2014 - mcdurdin - I4287 - V9.0 - Remove extraneous AppIntegration class type tests
                    23 Feb 2016 - mcdurdin - I4978 - Keyboard options do not apply correctly because they are set twice accidentally
*/
   // I4287
#ifndef _APPINT_H
#define _APPINT_H

typedef struct
{
	int ItemType;
	DWORD dwData;
} APPACTIONQUEUEITEM;

// QueueAction ItemTypes
#define QIT_VKEYDOWN	0
#define QIT_VKEYUP		1
#define QIT_VSHIFTDOWN	2
#define QIT_VSHIFTUP	3
#define QIT_CHAR		4
#define QIT_DEADKEY		5
#define QIT_BELL		6
#define QIT_BACK		7
#define QIT_CAPSLOCK 8
#define QIT_INVALIDATECONTEXT 9

#define QVK_EXTENDED 0x00010000 // Flag for QIT_VKEYDOWN to indicate an extended key
#define QVK_KEYMASK  0x0000FFFF
#define QVK_FLAGMASK 0xFFFF0000

#define MAXCONTEXT 64
#define MAXACTIONQUEUE	1024

class AppActionQueue
{
protected:
	APPACTIONQUEUEITEM Queue[MAXACTIONQUEUE];
	int QueueSize;

public:
	AppActionQueue();
	virtual void ResetQueue();
	virtual BOOL QueueAction(int ItemType, DWORD dwData);

  BOOL IsQueueEmpty() { return QueueSize == 0; }
  int GetQueueSize() { return QueueSize; }
};

class AppContext
{
private:
	WCHAR CurContext[MAXCONTEXT]; //!< CurContext[0] is furthest from the caret and buffer is null terminated.
	int pos;

public:
	AppContext();
  /**
   * Copy "source" AppContext to this AppContext
   *
   * @param source AppContext to copy
   */
  void CopyFrom(AppContext *source);

  /**
   * Add a single code unit to the Current Context. Not necessarily a complete code point
   *
   * @param Code unit to add
   */
  void Add(WCHAR ch);

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
	WCHAR *Buf(int n);

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

class AppIntegration:public AppActionQueue
{
protected:
	HWND hwnd;
	int FShiftFlags;
public:
	AppIntegration();

	//BOOL NoSetShift;

	/* Information functions */

	virtual BOOL CanHandleWindow(HWND ahwnd) = 0;
	virtual BOOL IsWindowHandled(HWND ahwnd) = 0;
	virtual BOOL HandleWindow(HWND ahwnd) = 0;
	virtual BOOL IsUnicode() = 0;

	/* Context functions */

	virtual void ReadContext() = 0;
	virtual void ResetContext() = 0;
  virtual void AddContext(WCHAR ch) = 0;  //I2436
	virtual WCHAR *ContextBuf(int n) = 0;
	virtual WCHAR *ContextBufMax(int n) = 0;

	/* Queue and sending functions */

	void SetCurrentShiftState(int ShiftFlags) { FShiftFlags = ShiftFlags; }
	virtual BOOL SendActions() = 0;   // I4196
};

/**
 * Convert AppContext array into an array of core context items.
 * Caller is responsible for freeing the memory.
 *
 * @param   buf     appcontext character array
 * @param   outPtr  The ouput array of context items. caller to free memory
 * @return  BOOL    True if array created successfully
 */
BOOL ContextItemsFromAppContext(WCHAR const* buf, km_core_context_item** outPtr);

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

extern const LPSTR ItemTypes[];

#endif
