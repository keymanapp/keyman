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

// QueueDebugInformation ItemTypes
#define QID_BEGIN_UNICODE		0
#define QID_BEGIN_ANSI			1
#define QID_GROUP_ENTER			2
#define QID_GROUP_EXIT			3
#define QID_RULE_ENTER			4
#define QID_RULE_EXIT			5
#define QID_MATCH_ENTER			6
#define QID_MATCH_EXIT			7
#define QID_NOMATCH_ENTER		8
#define QID_NOMATCH_EXIT		9
#define QID_END					10

#define QID_FLAG_RECURSIVE_OVERFLOW	0x0001
#define QID_FLAG_NOMATCH			0x0002

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
	WCHAR CurContext[MAXCONTEXT];
	int pos;

public:
	AppContext();
  void CopyFrom(AppContext *source);
	void Add(WCHAR ch);
	void Delete();
	void Reset();
	void Get(WCHAR *buf, int bufsize);
	void Set(const WCHAR *buf);
	WCHAR *BufMax(int n);
	WCHAR *Buf(int n);
	BOOL CharIsDeadkey();
  BOOL CharIsSurrogatePair();
};

class AppContextWithStores : public AppContext   // I4978
{
public:
  AppContextWithStores(int nKeyboardOptions);
  ~AppContextWithStores();
  DWORD nKeyboardOptions;
  LPINTKEYBOARDOPTIONS KeyboardOptions;
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
	
	virtual BOOL QueueDebugInformation(int ItemType, LPGROUP Group, LPKEY Rule, PWSTR fcontext, PWSTR foutput, DWORD_PTR dwExtraFlags) = 0;
	void SetCurrentShiftState(int ShiftFlags) { FShiftFlags = ShiftFlags; }
	virtual BOOL SendActions() = 0;   // I4196
};

extern const LPSTR ItemTypes[];
	
#endif

