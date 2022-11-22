
class XString
{
friend class XStringPointer;
private:
	int RefCount;	/* If RefCount > 0 when ~XString called, log an error */ 
	WCHAR *buf;		/* Buffer for XString */ 
	int nbuf;		/* Length of buf in bytes */ 
public:
	XString();							/* New, empty XString */ 
	XString(PWSTR FInitWStr);			/* New, copy from FInitWStr */ 
	XString(XString *FInitXStr);		/* New, copy from FInitXStr */ 
	~XString();
	int Length();						/* Length in characters */ 
	int WordLength() { return nbuf; }	/* Length in WORDs */ 

	void Append(XString *FAppendXStr);
	void Append(WCHAR *FAppendWStr);
	void Append(WCHAR FAppendChar);
	void Delete(int Position);				/* Delete the character at Position */ 
	void Delete(int Position, int Length);	/* Delete the character at Position to Pos+Len-1 */ 
};

class XStringPointer
{
private:
	XString str;
	WCHAR *p;

public:
	XStringPointer(XString FParent);
	~XStringPointer();

	operator++();
	operator--();
	WCHAR operator[](int n);
	WCHAR *cp() { return p; }
	BOOL IsSurrogate();
	BOOL IsFunction();
	BOOL IsFunction(int FunctionID);
	int Position();						/* Returns the current character position */ 
	int WordPosition();					/* Returns the current word position */ 
	void Insert(XString *FInsertXStr);	/* Insert a character before pointer position */ 
	void Insert(WCHAR *FInsertWStr);
	void Insert(WCHAR FInsertChar);
	void Delete();						/* Delete the current character */ 
	void Delete(int n);					/* Delete the current character + n-1 chrs */ 
};
