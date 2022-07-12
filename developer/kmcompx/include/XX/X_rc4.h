

#include "../../../../developer/kmcompx/include/kmcompx.h"
class CryptRC4
{
private:
	KMX_BOOL fInitialized;
	BYTE KeyData[256], KeyOrg[256];
	void EncryptBuffer(PBYTE InData, PBYTE OutData, KMX_DWORD Size);
	void DecryptBuffer(PBYTE InData, PBYTE OutData, KMX_DWORD Size);
public:
	static BOOL SelfTest();
	void Init(PBYTE Key, KMX_DWORD Size, PVOID InitVector);
	void Burn();
	void Reset();
	void Encrypt(PBYTE InData, PBYTE OutData, KMX_DWORD Size);
	void Decrypt(PBYTE InData, PBYTE OutData, KMX_DWORD Size);
};
