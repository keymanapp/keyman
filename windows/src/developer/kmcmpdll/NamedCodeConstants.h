
#ifndef _NAMEDCODECONSTANTS_H
#define _NAMEDCODECONSTANTS_H

#define MAX_ENAME	128
#define ALLOC_SIZE	256

struct NCCENTRY
{
	char name[MAX_ENAME+1];
	int code;
  DWORD storeIndex;
};

class NamedCodeConstants
{
private:
	NCCENTRY *entries;			// entries from &includecodes
	NCCENTRY *entries_file;		// entries from store(myconst) x
	int nEntries, nEntries_file;
	int chrindexes[128];		// A-Z, 0-9, -, _; simple index

	int GetCode_IncludedCodes(const char *codename);
	void AddCode_IncludedCodes(int n, char *p);
	BOOL IntLoadFile(const char *filename);
public:
	NamedCodeConstants();
	~NamedCodeConstants();

	void reindex();
	void AddCode(int n, char *p, DWORD storeIndex);
	BOOL LoadFile(const char *filename);
	int GetCode(const char *codename, DWORD *storeIndex);
};

#endif //_NAMEDCODECONSTANTS_H