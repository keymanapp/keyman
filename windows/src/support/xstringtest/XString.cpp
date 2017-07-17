
#include <windows.h>


struct XChar
{
	XCharType CharType;
	union
	{
		DWORD WideChar;
		struct { WORD n1, n2; }
	}
};

class XString
{
private:
	WCHAR *FBase;
	int BaseLength;

	void GetXChar(WCHAR *p, XChar *ch);
	WCHAR GetWChar(WCHAR *p);
public:
	/* functions starting with Base return underlying buffer information */ 
	XString();
	XString(XString *source);
	XString(WCHAR *source);

	void Set(XString *source);
	void Set(WCHAR *source);
	void Clear();

	int Length();
	int BaseLength();		

	void CharAt(int XPos, XChar *ch);
	WCHAR CharAt(int XPos);
	int IndexOf(XChar *ch);
	int IndexOf(WCHAR ch);
	
	void Insert(XString *str, int XPos);
	void Insert(WCHAR *str, int XPos);
	void Insert(XChar *ch, int XPos);
	void Insert(WCHAR ch, int XPos);
	void Append(XString *str);
	void Append(WCHAR *str);
	void Append(XChar *ch);
	void Append(WCHAR ch);
	void Delete(int XPos, int XLen);

	const WCHAR *Base();
};


XString::XString()
{
	FBase = NULL;
}

XString::XString(XString *source)
{
	Set(source);
}

XString::XString(WCHAR *source)
{
	Set(source);
}

void XString::Set(XString *source)
{
	Clear();
	FBase = new WCHAR[source->BaseLength()+1];
	if(FBase) wcscpy(FBase, source->FBase);
}


void XString::Set(WCHAR *source)
{
	Clear();
	FBase = new WCHAR[wcslen(source)+1];
	if(FBase) wcscpy(FBase, source);
}

void XString::Clear()
{
	if(FBase) delete FBase;
	FBase = NULL;
}

int XString::Length()
{
	if(!FBase) return 0;
	WCHAR *p;
	int i;
	for(i = 0, p = FBase; *p; p=incxstr(p), i++);
	return i;
}

int XString::BaseLength()
{
	if(!FBase) return 0;
	return wcslen(FBase);
}

void XString::CharAt(int XPos, XChar *ch)
{
	ch->CharType = ctNone;
	if(!FBase) return;
	WCHAR *p;
	int i;
	for(i = 0, p = FBase; *p && i < XPos; p=incxstr(p), i++);
	if(i < XPos) return;
	GetXChar(p, ch);
}

WCHAR XString::CharAt(int XPos)
{
	if(!FBase) return 0;
	WCHAR *p;
	int i;
	for(i = 0, p = FBase; *p && i < XPos; p=incxstr(p), i++);
	if(i < XPos) return 0;
	return GetWChar(p);
}

int XString::IndexOf(XChar *ch)
{
	if(!FBase) return -1;
	WCHAR *p;
	int i;
	for(i = 0, p = FBase; *p && !IsEqualChar(p, ch); p=incxstr(p), i++);
	if(!*p) return -1;
	return i;
}

int XString::IndexOf(WCHAR ch)
{
	if(!FBase) return -1;
	WCHAR *p;
	int i;
	for(i = 0, p = FBase; *p && *p != ch; p=incxstr(p), i++);
	if(!*p) return -1;
	return i;
}
	
void XString::Insert(XString *str, int XPos)
{
}

void XString::Insert(WCHAR *str, int XPos);
void XString::Insert(XChar *ch, int XPos);
void XString::Insert(WCHAR ch, int XPos);
void XString::Append(XString *str);
void XString::Append(WCHAR *str);
void XString::Append(XChar *ch);
void XString::Append(WCHAR ch);
void XString::Delete(int XPos, int XLen);

const WCHAR *Base();

/* Private functions */ 

	void GetXChar(WCHAR *p, XChar *ch);
	WCHAR GetWChar(WCHAR *p);
