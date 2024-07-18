
#ifndef NAMEDCODECONSTANTS_H
#define NAMEDCODECONSTANTS_H

#include "compfile.h"

#define MAX_ENAME	128
#define ALLOC_SIZE	256

struct NCCENTRY
{
	KMX_WCHAR name[MAX_ENAME+1];
	int code;
    KMX_DWORD storeIndex;
};

namespace kmcmp{
	class NamedCodeConstants
	{
	private:
		NCCENTRY *entries;			// entries from &includecodes
		NCCENTRY *entries_file;		// entries from store(myconst) x
		int nEntries, nEntries_file;
		int chrindexes[128];		// A-Z, 0-9, -, _; simple index

		int GetCode_IncludedCodes(const KMX_WCHAR *codename);
		void AddCode_IncludedCodes(int n, const KMX_WCHAR *p);
	public:
		NamedCodeConstants();
		~NamedCodeConstants();

		void reindex();
		void AddCode(int n, const KMX_WCHAR *p, KMX_DWORD storeIndex);
		KMX_BOOL LoadFile(PFILE_KEYBOARD fk, const KMX_WCHAR *filename);
		int GetCode(const KMX_WCHAR *codename, KMX_DWORD *storeIndex);
	};
}
#endif //NAMEDCODECONSTANTS_H
